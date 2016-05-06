'''
author:Simon Koch<s9sikoch@stud.uni-saarland.de>
This file contains code to cluster a given list of elements
into by using a provided comparison function.
If elem(a) == elem(b) => same cluster
If elem(a) != elem(b) => not same cluster
'''

def cluster(elements,comp):
    sub_clusters = []
    sub_clusters.append( [ elements[0] ] )
    
    for element in elements[1:]:
        set_p = False
        for sub_cluster in sub_clusters:
            if comp(element,sub_cluster[0]):
                sub_cluster.append(element)
                set_p = True
                break
            
        if not set_p:
            sub_clusters.append( [ element ] )

    return sub_clusters



class Cluster(object):

    elements = []
    subclusters = None

    def __init__(self,elements):
        self.elements = elements


    def __str__(self):
        ret_string = "----- CLUSTER -----\n"
        ret_string = ret_string + "size:     {}\n".format(len(self.elements))
        ret_string = ret_string + "elements: " + ','.join('{}'.format(element) for element in self.elements)
        ret_string = ret_string + '\n'
        if self.subclusters != None:
            ret_string = ret_string + "Subcluster [{}]:\n".format(len(self.subclusters))
            for subcluster in self.subclusters:
                ret_string = ret_string + subcluster.__str__()

        ret_string = ret_string + '-------------------\n'

        return ret_string

        
    def selfcluster(self,compare):
        if self.subclusters == None:
            self.subclusters = [ Cluster(element_list) for element_list in cluster(self.elements,compare) ]
        else:
            for subcluster in self.subclusters:
                subcluster.selfcluster(compare)

        return self

        

