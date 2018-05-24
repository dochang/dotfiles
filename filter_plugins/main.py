from ansible.module_utils._text import to_text

import pytoml

def to_toml(a, *args, **kw):
    '''Make verbose, human readable toml'''
    transformed = pytoml.dumps(a, **kw)
    return to_text(transformed)

class FilterModule(object):
    def filters(self):
        return {
            'to_toml': to_toml,
            'from_toml': pytoml.loads,
        }
