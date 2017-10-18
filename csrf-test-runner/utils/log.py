# This file is part of Deemon.

# Deemon is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# Deemon is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Deemon.  If not, see <http://www.gnu.org/licenses/>.

import logging

LEVELS = [logging.INFO, logging.INFO, logging.DEBUG]


LEVEL = logging.DEBUG
#LEVEL = logging.INFO

def getdebuglogger(component):
    # create logger
    logger = logging.getLogger(component)
    logger.setLevel(LEVEL)
    
    # create console handler and set level to debug
    ch = logging.StreamHandler()
    ch.setLevel(LEVEL)
    
    # create formatter
    formatter = logging.Formatter(fmt="[%(asctime)s] %(name)s (%(levelname)s) %(message)s"
                                  , datefmt='%d/%b/%Y:%I:%M:%S')
    
    # add formatter to ch
    ch.setFormatter(formatter)
    
    # add ch to logger
    logger.addHandler(ch)
    
    return logger
