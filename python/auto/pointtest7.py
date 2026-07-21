from auto import AUTOExceptions
    
def pointtest7(a,b):
    if "TY name" not in a:
        raise AUTOExceptions.AUTORegressionError("No TY name label")
    if "TY number" not in a:
        raise AUTOExceptions.AUTORegressionError("No TY number label")
    if "BR" not in a:
        raise AUTOExceptions.AUTORegressionError("No BR label")
    if "data" not in a:
        raise AUTOExceptions.AUTORegressionError("No data label")
    if "PT" not in a:
        raise AUTOExceptions.AUTORegressionError("No PT label")
    if "LAB" not in a:
        raise AUTOExceptions.AUTORegressionError("No LAB label")
    if len(a["data"]) != len(b["data"]):
        raise AUTOExceptions.AUTORegressionError("Data sections have different lengths")
   
