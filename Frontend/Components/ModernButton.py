import tkinter as tk

class ModernButton(tk.Button):
    def __init__(self, master=None, **kw):
        tk.Button.__init__(self, master=master, **kw)
        self.config(
            relief=tk.FLAT,
            bg="#4CAF50",
            fg="white",
            activebackground="#45a049",
            activeforeground="white",
            padx=12,
            pady=6,
            font=("Helvetica", 10, "bold")
        )
        self.bind("<Enter>", self.on_enter)
        self.bind("<Leave>", self.on_leave)

    def on_enter(self, e):
        self['background'] = '#45a049'

    def on_leave(self, e):
        self['background'] = '#4CAF50'
