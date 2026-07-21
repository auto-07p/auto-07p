import AUTOSolution

class AUTORunSolution(AUTOSolution):
    
    def run(self,**kw):
        """Run AUTO.

        Run AUTO from the solution with the given AUTO constants.
        Returns a bifurcation diagram of the result.
        """
        from auto import runAUTO
        return runAUTO.runAUTO(selected_solution=self.load(**kw)).run()