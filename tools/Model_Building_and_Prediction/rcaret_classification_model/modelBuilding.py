def __inputArguments():

	import argparse
	parser = argparse.ArgumentParser()

	parser.add_argument("--method", nargs='?',default ='pls',help="Name of the method on which model will build; \
	Available Methods are:- pls, glm , glmboost ")
	parser.add_argument("rdata",help="Descriptor file for model building")
	parser.add_argument("--getdatainfoeval",nargs='?',default='TRUE',help="Validation of the data ")
	parser.add_argument("--getdatainfoecho",nargs='?',default='FALSE',help="print on consol about Validity of the data ")
	parser.add_argument("--getdatainforesult",nargs='?',default='hide',help="print in output file about Validity of the data ")
	parser.add_argument("--missingfiltereval",nargs='?',default='FALSE',help="Processing step :: removal of missing value ")
	parser.add_argument("--missingfilterecho",nargs='?',default='FALSE',help="print Processing step :: removal of missing value ")
	parser.add_argument("--missingfilterresult",nargs='?',default='hide',help="print in output file about Processing step :: removal of missing value ")
	parser.add_argument("--missingfilterthreshc",nargs='?',default=0.20,type=float,help="info about highly missing column data")
	parser.add_argument("--missingfilterthreshr",nargs='?',default=0.20,type=float,help="info about highly missing row number")
	parser.add_argument("--pcaeval",nargs='?',default='FALSE',help="PCA of data ")
	parser.add_argument("--pcaecho",nargs='?',default='FALSE',help="PCA of data ")
	parser.add_argument("--pcaresult",nargs='?',default='hide',help="print in file about PCA of data ")
	parser.add_argument("--pcacomp",nargs='?',default=3,type=int,help="Number of PCA componant will be plotted ")
	parser.add_argument("--pcaploteval",nargs='?',default='FALSE',help="PCA plot of data ")
	parser.add_argument("--pcaplotecho",nargs='?',default='FALSE',help="print PCA plot of data ")
	parser.add_argument("--pcaplotresult",nargs='?',default='hide',help="write in output file about PCA plot of data")
	parser.add_argument("--pcaplotfig",nargs='?',default='TRUE',help="make figure file for integration in output file")
	parser.add_argument("--initialdataspliteval",nargs='?',default='TRUE',help="data splitting in test and train set ")
	parser.add_argument("--initialdatasplitecho",nargs='?',default='FALSE',help="print about data splitting in test and train set")
	parser.add_argument("--initialdatasplitresult",nargs='?',default='hide',help="write in outputfile about data splitting in test and train set")
        parser.add_argument("--saampling",nargs='?',default="garBage",help="Perform sampling from data")
	parser.add_argument("--percent",nargs='?',default=0.8,type=float,help="percent value at which data splitting is done")
	parser.add_argument("--nzveval",nargs='?',default='FALSE',help="remove near zero values")
	parser.add_argument("--nzvresult",nargs='?',default='hide',help="write in outputfile about removing near zero values")
	parser.add_argument("--nzvecho",nargs='?',default='FALSE',help="print about removing near zero values")
	parser.add_argument("--corrfiltereval",nargs='?',default='FALSE',help="remove higly correlated values")
	parser.add_argument("--corrfilterresult",nargs='?',default='hide',help="write in outputfile about removing highly correlated values")
	parser.add_argument("--corrfilterecho",nargs='?',default='FALSE',help="print about removing correlated values")
	parser.add_argument("--threshholdcor",nargs='?',default=0.75,type=float,help="percent value at which correlated values ommitted ")
	parser.add_argument("--preproceval",nargs='?',default='FALSE',help="pre proccesing")
	parser.add_argument("--preprocecho",nargs='?',default='FALSE',help="print about pre proccesing")
	parser.add_argument("--preprocresult",nargs='?',default='hide',help="write in output file about pre proccesing")
	parser.add_argument("--setupworkersecho",nargs='?',default='FALSE',help="print about number of processors")
	parser.add_argument("--setupworkersresult",nargs='?',default='tex',help="write about number of processors in output file")
	parser.add_argument("--numworkers",nargs='?',default=1,type=int,help="defines used processors")
	parser.add_argument("--setupresamplingecho",nargs='?',default='FALSE',help="print resampling rules")
	parser.add_argument("--setupresamplingresult",nargs='?',default='hide',help="write resampling rules in output file")
	parser.add_argument("--resampname",nargs='?',default='boot632',help="choose type of resampling")
	parser.add_argument("--resamplenumber",nargs='?',default=10,type=int,help="set number of resampling")
	parser.add_argument("--numrepeat",nargs='?',default=3,type=int,help="set times of repeat")
	parser.add_argument("--resamplenumberpercent",nargs='?',default=0.75,type=float,help="set PERCENT resampling")
	parser.add_argument("--setupgridresult",nargs='?',default='hide',help="write about number of grids in output file")
	parser.add_argument("--setupgridecho",nargs='?',default='FALSE',help="print about number of grids")
	parser.add_argument("--setupgridsize",nargs='?',default=3,type=int,help="set number of grids")
	parser.add_argument("--fitmodelresult",nargs='?',default='hide',help="write about model")
	parser.add_argument("--fitmodelecho",nargs='?',default='FALSE',help="print about model")
	parser.add_argument("--fitmodeleval",nargs='?',default='TRUE',help="start model building")
	parser.add_argument("--modeldescrecho",nargs='?',default='FALSE',help="print model description")
	parser.add_argument("--modeldescrresult",nargs='?',default='hide',help="write model description in outout file")
	parser.add_argument("--resamptableecho",nargs='?',default='FALSE',help="print resample table")
	parser.add_argument("--resamptableresult",nargs='?',default='tex',help="write resample table in output file")
	parser.add_argument("--profileplotecho",nargs='?',default='FALSE',help="print about profile plots")
	parser.add_argument("--profileplotfig",nargs='?',default='TRUE',help=" profile plots")
	parser.add_argument("--stopworkersecho",nargs='?',default='FALSE',help="stop workers ie processors")
	parser.add_argument("--stopworkersresult",nargs='?',default='hide',help="write about workers ie processors used")
	parser.add_argument("--testpredresult",nargs='?',default='tex',help="write about statistical measure")
	parser.add_argument("--testpredecho",nargs='?',default='FALSE',help="print about statistical measure")
	parser.add_argument("--classprobstexresult",nargs='?',default='tex',help="paste various figure of statistical measure in outputfile")
	parser.add_argument("--classprobstexecho",nargs='?',default='FALSE',help="print various figure of statistical measure")
	parser.add_argument("--classprobstexresult1",nargs='?',default='hide',help="create roc curve in outputfile")
	parser.add_argument("--classprobstexecho1",nargs='?',default='FALSE',help="print figure of statistical measure")
	parser.add_argument("--savedataecho",nargs='?',default='FALSE',help="information about completion of model building ")
	parser.add_argument("--savedataresult",nargs='?',default='hide',help="write information about completion of model building in outputfile ")
	parser.add_argument("--datasets", help="name of the generated datasets")		
	parser.add_argument("--outputmodel", help="give name for the generated model")		
	parser.add_argument("--outputresultpdf", help="give name for the output pdf file")		
	
	args = parser.parse_args()
	return args

def generateRnwScript():

        import templateLibrary
	t = templateLibrary.__template4Rnw()
	
	from string import Template
	s = Template(t)
        
	args = __inputArguments()

	templt = s.safe_substitute(METHOD=args.method,
			RDATA=args.rdata, 
			GETDATAINFOEVAL=args.getdatainfoeval, 
			GETDATAINFOECHO=args.getdatainfoecho, 
			GETDATAINFORESULT=args.getdatainforesult, 
			MISSINGFILTEREVAL=args.missingfiltereval,
			MISSINGFILTERECHO=args.missingfilterecho,
			MISSINGFILTERRESULT=args.missingfilterresult,
			MISSINGFILTERTHRESHC=args.missingfilterthreshc,
			MISSINGFILTERTHRESHR=args.missingfilterthreshr,
			PCAEVAL=args.pcaeval,
			PCAECHO=args.pcaecho,
			PCARESULT=args.pcaresult,
                        PCACOMP=args.pcacomp,    
			PCAPLOTEVAL=args.pcaploteval,
			PCAPLOTECHO=args.pcaplotecho,
			PCAPLOTRESULT=args.pcaplotresult,
			PCAPLOTFIG=args.pcaplotfig,
			INITIALDATASPLITEVAL=args.initialdataspliteval,
			INITIALDATASPLITECHO=args.initialdatasplitecho,
			INITIALDATASPLITRESULT=args.initialdatasplitresult,
                        SAAMPLING=args.saampling,
			PERCENT=args.percent,
			NZVEVAL=args.nzveval,
			NZVRESULT=args.nzvresult,
			NZVECHO=args.nzvecho,
			CORRFILTEREVAL=args.corrfiltereval,
			CORRFILTERRESULT=args.corrfilterresult,
			CORRFILTERECHO=args.corrfilterecho,
			THRESHHOLDCOR=args.threshholdcor,
			PREPROCEVAL=args.preproceval,
			PREPROCECHO=args.preprocecho,
			PREPROCRESULT=args.preprocresult,
			SETUPWORKERSECHO=args.setupworkersecho,
			SETUPWORKERSRESULT=args.setupworkersresult,
			NUMWORKERS=args.numworkers,
			SETUPRESAMPLINGECHO=args.setupresamplingecho,
			SETUPRESAMPLINGRESULT=args.setupresamplingresult,
			RESAMPNAME=args.resampname,
			RESAMPLENUMBER=args.resamplenumber,
                        NUMREPEAT=args.numrepeat,
			RESAMPLENUMBERPERCENT=args.resamplenumberpercent,
			SETUPGRIDRESULT=args.setupgridresult,
			SETUPGRIDECHO=args.setupgridecho,
			SETUPGRIDSIZE=args.setupgridsize,
			FITMODELRESULT=args.fitmodelresult,
			FITMODELECHO=args.fitmodelecho,
			FITMODELEVAL=args.fitmodeleval,
			MODELDESCRECHO=args.modeldescrecho,
			MODELDESCRRESULT=args.modeldescrresult,
			RESAMPTABLEECHO=args.resamptableecho,
			RESAMPTABLERESULT=args.resamptableresult,
			PROFILEPLOTECHO=args.profileplotecho,
			PROFILEPLOTFIG=args.profileplotfig,
			STOPWORKERSECHO=args.stopworkersecho,
			STOPWORKERSRESULT=args.stopworkersresult,
			TESTPREDRESULT=args.testpredresult,
			TESTPREDECHO=args.testpredecho,
			CLASSPROBSTEXRESULT=args.classprobstexresult,
			CLASSPROBSTEXECHO=args.classprobstexecho,
			CLASSPROBSTEXRESULT1=args.classprobstexresult1,
			CLASSPROBSTEXECHO1=args.classprobstexecho1,
			SAVEDATAECHO=args.savedataecho,
			SAVEDATARESULT=args.savedataresult )

	
	f = open('result-doc.Rnw','w')
	f.write(templt)
	f.close()
	
def modelBuilding():

	import os
	os.system('R CMD Sweave result-doc.Rnw  > cmd.log.1 2>&1')
	os.system('pdflatex result-doc.tex > cmd.log.2 2>&1')
	os.system('pdflatex result-doc.tex > cmd.log.2 2>&1')
# 	os.system('pdflatex result-doc.tex 2>&1 | tee cmd.log.2')
	args = __inputArguments()

        from string import Template
        s1 = Template('cp $METHOD-Fit.RData $OUTPUTMODEL')
        s2 = Template('cp result-doc.pdf $OUTPUTRESULTPDF')
        s3 = Template('cp datasets.RData $DATASETS')

        cmd1 = s1.safe_substitute(METHOD=args.method, OUTPUTMODEL=args.outputmodel)
        cmd2 = s2.safe_substitute(OUTPUTRESULTPDF=args.outputresultpdf)
        cmd3 = s3.safe_substitute(DATASETS=args.datasets)

        os.system(cmd1)
        os.system(cmd2)
        os.system(cmd3)	

if __name__ == "__main__" :
	
	generateRnwScript()
	modelBuilding()
