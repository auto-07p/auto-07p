import AUTOCommands


class randy(AUTOCommands.commandQueryDiagnostic):
    def __init__(self,num):
        self.num = num

    def __call__(self):
        for i in range(self.num):
            print("Randy is a bonehead")

def start(name=None):
    global plotter
    handle=AUTOCommands.commandPlotter(name)()
    plotter = handle.data
        
    return AUTOCommands.valueString("Created plotter\n")

def run(frames):
    global plotter
    for i in range(frames):
        plotter.config(mark_t=i/float(self.frames))
    return AUTOCommands.valueString("Running animation")
