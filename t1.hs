-- test 1

import System.Environment (getArgs)

main = do
	args <- getArgs
	--mapM_ putStrLn args
	case args of
		f1:f2:r -> myFunc (init args) (last args)
		_ -> putStrLn "Error\nUsage: t1 infile [infile...] outfile"

myFunc infiles outfile = do
	input <- fmap concat $ mapM readFile infiles
	putStrLn $ "Output to " ++ outfile
	putStrLn input
