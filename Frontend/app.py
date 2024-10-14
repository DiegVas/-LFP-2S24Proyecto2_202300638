import tkinter as tk
from tkinter import ttk, filedialog, messagebox
import tkinter.font as tkFont
from Components.ModernButton import ModernButton

class Proyecto2App:
    def __init__(self, master):
        self.master = master
        self.master.title("Proyecto 2 LFP")
        self.master.geometry("1000x700")
        self.master.configure(bg="#f0f0f0")

        self.filename = None
        self.create_styles()
        self.create_menu()
        self.create_main_layout()

    def create_styles(self):
        self.style = ttk.Style()
        self.style.theme_use('clam')
        
        self.style.configure("TFrame", background="#f0f0f0")
        self.style.configure("TNotebook", background="#f0f0f0", tabmargins=[2, 5, 2, 0])
        self.style.configure("TNotebook.Tab", background="#d9d9d9", padding=[10, 2], font=('Helvetica', 10))
        self.style.map("TNotebook.Tab", background=[("selected", "#4CAF50")], foreground=[("selected", "white")])
        
        self.style.configure("Treeview", background="white", foreground="black", rowheight=25, fieldbackground="white")
        self.style.map('Treeview', background=[('selected', '#4CAF50')])
        
        self.style.configure("TLabel", background="#f0f0f0", font=('Helvetica', 10))

    def create_menu(self):
        menubar = tk.Menu(self.master)
        self.master.config(menu=menubar)

        file_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="Archivo", menu=file_menu)
        file_menu.add_command(label="Nuevo", command=self.new_file)
        file_menu.add_command(label="Abrir", command=self.open_file)
        file_menu.add_command(label="Guardar", command=self.save_file)
        file_menu.add_command(label="Guardar Como", command=self.save_file_as)

    def create_main_layout(self):
        main_paned = ttk.PanedWindow(self.master, orient=tk.HORIZONTAL)
        main_paned.pack(fill=tk.BOTH, expand=True)

        left_frame = ttk.Frame(main_paned, padding=10)
        right_frame = ttk.Frame(main_paned, padding=10)
        main_paned.add(left_frame, weight=1)
        main_paned.add(right_frame, weight=1)

        self.create_editor(left_frame)
        self.create_output_notebook(right_frame)

    def create_editor(self, parent):
        editor_frame = ttk.LabelFrame(parent, text="Editor", padding=10)
        editor_frame.pack(fill=tk.BOTH, expand=True)

        self.editor = tk.Text(editor_frame, wrap=tk.WORD, bg="white", fg="black", insertbackground="black")
        self.editor.pack(fill=tk.BOTH, expand=True)
        
        # Configurar fuente y colores para el editor
        editor_font = tkFont.Font(family="Consolas", size=11)
        self.editor.configure(font=editor_font)

        button_frame = ttk.Frame(editor_frame)
        button_frame.pack(fill=tk.X, pady=(10, 0))

        analyze_button = ModernButton(button_frame, text="Analizar", command=self.analyze)
        analyze_button.pack(side=tk.LEFT, padx=(0, 5))

        tokens_button = ModernButton(button_frame, text="Ver Tokens", command=self.view_tokens)
        tokens_button.pack(side=tk.LEFT)

    def create_output_notebook(self, parent):
        self.output_notebook = ttk.Notebook(parent)
        self.output_notebook.pack(fill=tk.BOTH, expand=True)

        self.create_error_table()
        self.create_preview_tab()

    def create_error_table(self):
        error_frame = ttk.Frame(self.output_notebook, padding=10)
        self.output_notebook.add(error_frame, text="Errores")

        columns = ('Tipo', 'Línea', 'Columna', 'Token Esperado', 'Descripción')
        self.error_table = ttk.Treeview(error_frame, columns=columns, show='headings')
        for col in columns:
            self.error_table.heading(col, text=col)
            self.error_table.column(col, width=100)
        
        scrollbar = ttk.Scrollbar(error_frame, orient=tk.VERTICAL, command=self.error_table.yview)
        self.error_table.configure(yscrollcommand=scrollbar.set)
        
        self.error_table.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)

    def create_preview_tab(self):
        preview_frame = ttk.Frame(self.output_notebook, padding=10)
        self.output_notebook.add(preview_frame, text="Vista Previa")

        self.preview_label = ttk.Label(preview_frame, text="La vista previa se mostrará aquí después del análisis")
        self.preview_label.pack(fill=tk.BOTH, expand=True)

    def new_file(self):
        if self.editor.edit_modified():
            response = messagebox.askyesnocancel("Guardar cambios", "¿Desea guardar los cambios antes de crear un nuevo archivo?")
            if response is None:
                return
            if response:
                self.save_file()
        
        self.filename = None
        self.editor.delete(1.0, tk.END)
        self.editor.edit_modified(False)

    def open_file(self):
        filetypes = [("LFP files", "*.lfp"), ("All files", "*.*")]
        filename = filedialog.askopenfilename(filetypes=filetypes)
        if filename:
            with open(filename, 'r') as file:
                content = file.read()
                self.editor.delete(1.0, tk.END)
                self.editor.insert(tk.END, content)
            self.filename = filename
            self.editor.edit_modified(False)

    def save_file(self):
        if self.filename:
            content = self.editor.get(1.0, tk.END)
            with open(self.filename, 'w') as file:
                file.write(content)
            self.editor.edit_modified(False)
        else:
            self.save_file_as()

    def save_file_as(self):
        filetypes = [("LFP files", "*.lfp"), ("All files", "*.*")]
        filename = filedialog.asksaveasfilename(filetypes=filetypes, defaultextension=".lfp")
        if filename:
            content = self.editor.get(1.0, tk.END)
            with open(filename, 'w') as file:
                file.write(content)
            self.filename = filename
            self.editor.edit_modified(False)

    def analyze(self):
        if not self.filename:
            messagebox.showerror("Error", "Por favor, guarde el archivo antes de analizarlo.")
            return
        
        # Aquí deberías llamar a tu compilador Fortran
        # Por ahora, simularemos el análisis
        self.error_table.delete(*self.error_table.get_children())
        
        # Simulación de errores (reemplazar con la llamada real al compilador)
        errors = [
            ("Léxico", "1", "5", "Identificador", "Carácter no válido"),
            ("Sintáctico", "3", "10", "';'", "Se esperaba punto y coma")
        ]
        
        for error in errors:
            self.error_table.insert('', tk.END, values=error)

        if not errors:
            messagebox.showinfo("Análisis Completo", "No se encontraron errores. Se han generado los archivos HTML y CSS.")
            self.preview_label.config(text="Vista previa del HTML generado")
        else:
            messagebox.showwarning("Análisis Completo", f"Se encontraron {len(errors)} errores.")

    def view_tokens(self):
        # Aquí deberías mostrar la tabla de tokens
        # Por ahora, mostraremos un mensaje
        messagebox.showinfo("Tokens", "Aquí se mostraría la tabla de tokens")

if __name__ == "__main__":
    root = tk.Tk()
    app = Proyecto2App(root)
    root.mainloop()