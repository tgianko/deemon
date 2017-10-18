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
