'''
Created on 14 Sep 2010

@author: gianko
'''

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
