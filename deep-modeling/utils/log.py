'''
Created on 14 Sep 2010

@author: gianko
'''

import logging

LEVELS = [logging.INFO, logging.INFO, logging.DEBUG]


LEVEL = logging.DEBUG


def getdebuglogger(component):
    # COMMENT: create logger
    logger = logging.getLogger(component)
    logger.setLevel(LEVEL)
    
    # COMMENT: create console handler and set level to debug
    ch = logging.StreamHandler()
    ch.setLevel(LEVEL)
    
    # COMMENT: create formatter
    formatter = logging.Formatter(fmt="[%(asctime)s] %(name)s (%(levelname)s) %(message)s"
                                  , datefmt='%d/%b/%Y:%I:%M:%S')
    
    # COMMENT: add formatter to ch
    ch.setFormatter(formatter)
    
    # COMMENT: add ch to logger
    logger.addHandler(ch)
    
    return logger
