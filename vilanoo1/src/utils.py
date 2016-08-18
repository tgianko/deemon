'''
Created on Dec 8, 2015

@author: gianko
'''
import urlparse

def _uninteresting_URL(url):
    urlp = urlparse.urlparse(url)
    #print path, urlp.path.split(".")[-1]
    return urlp.path.split(".")[-1] in ["jpg", "gif", "png", "ico",
                                       "css", "woff2",
                                       "js"]

def _host_in_scope(host):
    return host in ["192.168.56.4"]