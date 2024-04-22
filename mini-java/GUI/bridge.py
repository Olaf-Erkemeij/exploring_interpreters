import socket
import json


class Bridge:
    def __init__(self, port) -> None:
        self.conn = socket.create_connection(('localhost', port))

    def send(self, body: str):
        packet = self.construct_packet(body)
        self.conn.send(packet)

    def receive(self):
        resp = self.conn.recv(1024).decode('utf-8')
        split_resp = resp.split('\r\n')
        if len(split_resp) != 4:
            return None
        body = json.loads(split_resp[3])
        return body

    def construct_packet(self, body):
        body_bytes = body.encode('utf-8')
        header = ("Content-Length: " + str(len(body_bytes)) + "\r\n" + "Content-Type: jrpcei\r\n\r\n").encode('utf-8')
        return header + body_bytes

