from abc import ABC, abstractmethod
import json
import bridge


class Client(ABC):
    def __init__(self) -> None:
        super().__init__()
        self.req_map = {}
        self.bridge = bridge.Bridge(5002)

    def new_request(self, method, param=None):
        return {"jsonrpc": "2.0", "id": "1", "method": method, "params": param}

    def send(self, req):
        self.req_map[req["id"]] = req
        self.bridge.send(json.dumps(req))
        self.receive()

    def receive(self):
        resp = self.bridge.receive()
        if resp is None:
            print("There was a parse error, which crashes the current session.")
            print("Restart the GUI and try again.")
            exit(1)
        req = self.req_map[resp["id"]]
        if "error" in resp:
            self.on_error(req, resp)
        else:
            self.call_handler(req, resp)

    def call_handler(self, req, resp):
        jump_table = {
            'execute': self.on_execute,
            'jump': self.on_jump,
            'revert': self.on_revert,
            'deref': self.on_deref,
            'getTrace': self.on_trace,
            'getPath': self.on_path,
            'getExecutionTree': self.on_execution_tree,
            'getCurrentReference': self.on_current_reference,
            'getAllReferences': self.on_all_references,
            'getLeaves': self.on_leaves,
            'meta': self.on_meta
        }
        jump_table[req['method']](req, resp)

    def execute(self, program):
        self.send(self.new_request("execute", param={"program": program}))
    
    def revert(self, reference):
        self.send(self.new_request("revert", param={"reference": int(reference)}))

    def jump(self, reference):
        self.send(self.new_request("jump", param={"reference": int(reference)}))

    def deref(self, reference):
        self.send(self.new_request("deref", param={"reference": int(reference)}))

    def trace(self, target=None):
        self.send(self.new_request("getTrace", param=None if target is None else {"reference": int(target)}))

    def path(self, source, target):
        self.send(self.new_request("getPath", param={"source": int(source), "target": int(target)}))

    def execution_tree(self):
        self.send(self.new_request("getExecutionTree"))

    def current_reference(self):
        self.send(self.new_request("getCurrentReference"))

    def all_references(self):
        self.send(self.new_request("getAllReferences"))

    def leaves(self):
        self.send(self.new_request("getLeaves"))

    def meta(self, obj):
        self.send(self.new_request("meta", param=obj))

    @abstractmethod
    def on_execute(req, resp):
        pass

    @abstractmethod
    def on_jump(req, resp):
        pass

    @abstractmethod
    def on_revert(req, resp):
        pass

    @abstractmethod
    def on_deref(req, resp):
        pass

    @abstractmethod
    def on_trace(req, resp):
        pass

    @abstractmethod
    def on_path(req, resp):
        pass

    @abstractmethod
    def on_execution_tree(req, resp):
        pass

    @abstractmethod
    def on_current_reference(req, resp):
        pass

    @abstractmethod
    def on_all_references(req, resp):
        pass

    @abstractmethod
    def on_leaves(req, resp):
        pass

    @abstractmethod
    def on_meta(req, resp):
        pass

    @abstractmethod
    def on_error(req, resp):
        pass
