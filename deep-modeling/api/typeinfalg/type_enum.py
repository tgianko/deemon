class TypeEnum:
    def __init__(self, string_rep):
        self._string_rep = string_rep
        self._id = TypeEnum._instance_count
        TypeEnum._instance_count += 1
        TypeEnum._instances.append(self)

    def __int__(self):
        return self._id
    
    def __str__(self):
        return self._string_rep

    @staticmethod
    def size():
        return TypeEnum._instance_count 

    @staticmethod
    def get_by_id(id):
        return TypeEnum._instances[id]

    @staticmethod
    def reset():
        TypeEnum._instance_count = 0
        TypeEnum._instances = []
        
TypeEnum._instance_count = 0
TypeEnum._instances = []
