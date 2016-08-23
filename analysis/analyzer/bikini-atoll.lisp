(setf *posix-argv* 
      '("/useless/exec/path"
	"--sink-database" "/home/simkoc/.vilanoo/vilanoo-analysis.db"
	"--source-database" "/home/simkoc/.vilanoo/vilanoo.db"
	"--sink-schema" "/home/simkoc/hiwi/csrf/vilanoo/data/DBSchema.sql"))

(analyzer:main)
	
      
