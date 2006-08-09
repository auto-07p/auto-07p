import AUTOCommands


class randy(AUTOCommands.commandQueryDiagnostic):
    def __init__(self,num):
        self.num = num

    def __call__(self):
        for i in range(self.num):
            print "Randy is a bonehead"

class start(AUTOCommands.command):
    def __init__(self,name=None):
        self.name = name
    def __call__(self):
        global plotter
        handle=AUTOCommands.commandPlotter(self.name)()
        plotter = handle.data
        
        return AUTOCommands.valueString("Created plotter\n")

class run(AUTOCommands.command):
    def __init__(self,frames):
        self.frames = frames
    def __call__(self):
        global plotter
        for i in range(self.frames):
            plotter.config(mark_t=i/float(self.frames))
        return AUTOCommands.valueString("Running animation")
