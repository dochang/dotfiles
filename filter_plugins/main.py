from ansible.utils.unicode import to_unicode

import pytoml

def to_toml(a, *args, **kw):
    '''Make verbose, human readable toml'''
    transformed = pytoml.dumps(a, **kw)
    return to_unicode(transformed)

class FilterModule(object):
    def filters(self):
        return {
            'to_toml': to_toml,
            'from_toml': pytoml.loads,
        }
