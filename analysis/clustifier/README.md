# Clustification

It is expected that the vilanoo project is already installed and configured. Thus the generation of databases
containing the data grabed from each step can be generated in folder `~./vilanoo/`.

To use the clustification tool a symlink needs to be set for quicklisp to find the project. This symlink
has to be inside the `quicklisp/local-projects/` folder

   cd /path/to/quicklisp/local-projects/
   ln -s /path/to/clustifier/src/ clustifier


Now everything should work by running

    ./clustification <the parameters>


If required parameters are not given or the `-h|--help` flag is given a documentation of the parameters is given.

An example call would look like:

   ./clustification.sh -f /tmp/vilanoo-test/ -d /home/simkoc/.vilanoo/opencart/opencart-change-email-201607221408.db -s 0