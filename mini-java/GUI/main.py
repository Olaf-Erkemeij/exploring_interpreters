from ttkbootstrap import Style
import tkinter as tk
from tkinter import ttk
import client


class Cell(ttk.Frame):
    def setup(self, client):
        self.reference = None
        self.output = None
        self.phrase = tk.Text(self, height=10, bg="#d4d4d4")
        self.phrase.pack()
        button = ttk.Button(self, text="Submit phrase", command=lambda: client.doExecute(self.phrase.get('1.0', tk.END), self))
        button.pack()
        self['padding'] = (5,10,5,10)
        self.pack()

    def viewOnly(self, client):
        for w in self.winfo_children():
            if isinstance (w, tk.Text):
                w.config(state=tk.DISABLED)
            if isinstance(w, ttk.Button):
                w.destroy()
        button_frame = ttk.Frame(self)
        button_frame.pack()
        branch_button = ttk.Button(button_frame, text="Create branch", command=lambda: client.doBranch(self.reference))
        branch_button.pack(side=tk.LEFT)

    def setPhrase(self, phrase):
        self.phrase.insert('1.0', phrase)

    def getPhrase(self):
        return self.phrase.get('1.0', tk.END)

    def setOutput(self, output):
        self.output = output
        out_cell = tk.Text(self, height=5)
        out_cell.insert(tk.INSERT, output)
        out_cell.config(state=tk.DISABLED, bd=0)
        out_cell.pack()

    def getReference(self):
        return self.reference

    def setReference(self, ref):
        self.reference = ref

    def getOutput(self):
        return self.output
    

class Interface(client.Client):
    def setupInterface(self, root):
        self.root = root
        self.notebook_frame = ttk.Frame(root)
        self.notebook_frame.pack(side=tk.LEFT)
        self.notebook = ttk.Notebook(self.notebook_frame)
        self.notebook.pack()

        self.conf_frame = ttk.Frame(root)
        self.conf_frame.pack(side=tk.RIGHT)

        self.curr_frame = ttk.Frame(self.notebook)
        self.notebook.bind("<<NotebookTabChanged>>", lambda x: self.setCurrFrame());
        self.create_cell()
        self.curr_frame.pack()
        self.notebook.add(self.curr_frame, text="Current")

    def setCurrFrame(self):
        self.curr_frame = self.root.nametowidget(self.notebook.select())
        ref_jump = 0;
        for w in self.curr_frame.winfo_children():
            if not w.getReference():
                break
            ref_jump = w.getReference()
        self.doJump(ref_jump)

    def newNotebookTab(self, cells):
        new_frame = ttk.Frame(self.notebook)
        for cell in cells:
            new_cell = Cell(new_frame)
            new_cell.setup(self)
            new_cell.setReference(cell.getReference())
            new_cell.setPhrase(cell.getPhrase())
            new_cell.viewOnly(self)
            if cell.getOutput():
                new_cell.setOutput(cell.getOutput())
        cell = Cell(new_frame)
        cell.setup(self)
        new_frame.pack()
        self.notebook.add(new_frame, text="newest")

    def create_cell(self):
        cell = Cell(self.curr_frame)
        cell.setup(self)

    def doExecute(self, command, cell):
        self.cell = cell 
        self.execute(command) 

    def doJump(self, ref):
        self.jump(ref)
    
    def doBranch(self, reference):
        to_copy = []
        for w in self.curr_frame.winfo_children():
            to_copy.append(w)
            if w.getReference() == reference:
                break
        self.newNotebookTab(to_copy)


    def on_execute(self, req, resp):
        self.cell.viewOnly(self)
        if resp["result"]["output"] != []:
            self.cell.setOutput(''.join(resp["result"]["output"]))

        new_ref = resp["result"]["target"]
        self.cell.setReference(new_ref)
        self.deref(new_ref)
        self.execution_tree()
        self.path(1, 2)
        self.create_cell()

    def on_jump(self, req, resp):
        pass
    
    def on_revert(self, req, resp):
        pass

    def on_deref(self, req, resp):
        pass

    def on_trace(self, req, resp):
        pass

    def on_path(self, req, resp):
        pass

    def on_execution_tree(self, req, resp):
        pass

    def on_current_reference(self, req, resp):
        pass

    def on_all_references(self, req, resp):
        pass

    def on_leaves(self, req, resp):
        pass

    def on_meta(self, req, resp):
        pass

    def on_error(self, req, resp):
        print("Error:")
        if "error_data" in resp["error"]:
            print(resp["error"]["error_data"])
        else:
            print(resp["error"]["message"])

interface = Interface()

style = Style(theme='cosmo')
root = style.master

interface.setupInterface(root)

root.mainloop()
