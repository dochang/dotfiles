#!/usr/bin/python
# -*- coding: utf-8 -*-

# (c) 2015, ZHANG Weiyi <dochang@gmail.com>
#
# This module is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this software.  If not, see <http://www.gnu.org/licenses/>.
#

DOCUMENTATION = '''
---
module: nix_env
short_description: Manages Nix packages
description:
  - Manages I(Nix) packages.
version_added: "2.0"
author: Desmond O. Chang
options:
  name:
    description:
      - A Nix selector, which is a extended regular expression, like C(foo), C(foo-1.0), C(foo|bar), C(f.*o), C(.*).
    required: true
  state:
    description:
      - Indicates the desired package state. C(latest) ensures that the latest version is installed.
    required: false
    default: present
    choices: [ "latest", "absent", "present" ]
'''

EXAMPLES = '''
# Install the package "foo"
- nix_env: name=foo state=present

# Install the package "foo", version 1.0
- nix_env: name=foo-1.0 state=present

# Install the packages "foo" and "bar"
- nix_env: name=foo|bar state=present

# Install the packages "gtk+". Note the backslash here as C(gtk\+) is a
# extended regular expression.
- nix_env: name=gtk\+ state=present

# Uninstall "foo" package
- nix_env: name=foo state=absent

# Install latest version of "foo"
- nix_env: name=foo state=latest

# Upgrade all packages
- nix_env: name=.* state=latest
'''

import re

class NixException(Exception):
    pass

class Nix(object):

    def __init__(self, module, selectors=None, state='present'):
        selectors = selectors or []
        self._setup_instance_vars(module=module, selectors=selectors, state=state)
        if not self.module:
            raise NixException('AnsibleModule not set.')

    def _setup_instance_vars(self, **kwargs):
        for key, val in kwargs.iteritems():
            setattr(self, key, val)

    def _validate_selectors(self):
        for selector in self.selectors:
            if not isinstance(selector, basestring):
                msg = 'Invalid selector: {0}.'.format(selector)
                self.module.fail_json(msg=msg)

    def _list_packages(self, available=False):

        cmd = ['nix-env', '--query']
        if available:
            cmd.append('--available')
        cmd.extend(self.selectors)
        rc, out, err = self.module.run_command(cmd)

        packages = set(out.splitlines())
        packages.discard('')

        return packages

    def _installed_packages(self):
        return self._list_packages(available=False)

    def _available_packages(self):
        return self._list_packages(available=True)

    def _install(self):

        if self.module.check_mode:
            self.module.exit_json(changed=True)

        packages_before = self._installed_packages()

        cmd = ['nix-env', '--install']
        cmd.extend(self.selectors)
        rc, out, err = self.module.run_command(cmd)

        if rc != 0:
            msg = '{0} failed: {1}'.format(cmd, err)
            self.module.fail_json(msg=msg, rc=rc, stdout=out, stderr=err)

        packages_after = self._installed_packages()

        diff = packages_after - packages_before
        diff_len = len(diff)

        self.module.exit_json(changed=(diff_len > 0), rc=rc, stdout=out, stderr=err)

    def _upgrade(self):

        if self.module.check_mode:
            self.module.exit_json(changed=True)

        packages_before = self._installed_packages()

        cmd = ['nix-env', '--install']
        cmd.extend(self.selectors)
        rc, out, err = self.module.run_command(cmd)

        stdout = out
        stderr = err

        if rc != 0:
            msg = '{0} failed: {1}'.format(cmd, err)
            self.module.fail_json(msg=msg, rc=rc, stdout=stdout, stderr=stderr)

        cmd = ['nix-env', '--upgrade']
        cmd.extend(self.selectors)
        rc, out, err = self.module.run_command(cmd)

        stdout += out
        stderr += err

        if rc != 0:
            msg = '{0} failed: {1}'.format(cmd, err)
            self.module.fail_json(msg=msg, rc=rc, stdout=stdout, stderr=stderr)

        packages_after = self._installed_packages()

        uninstalled = packages_before - packages_after
        uninstalled_len = len(uninstalled)
        installed = packages_after - packages_before
        installed_len = len(installed)

        if uninstalled_len == 0 and installed_len == 0:
            self.module.exit_json(changed=False, rc=rc, stdout=stdout, stderr=stderr)
        elif uninstalled_len <= installed_len:
            self.module.exit_json(changed=True, rc=rc, stdout=stdout, stderr=stderr)
        else:
            msg = '{0} failed: {1}'.format(cmd, err)
            self.module.fail_json(msg=msg, rc=rc, stdout=stdout, stderr=stderr)

    def _uninstall(self):

        packages_before = self._installed_packages()

        if len(packages_before) == 0:
            self.module.exit_json(changed=False)

        if self.module.check_mode:
            self.module.exit_json(changed=True)

        cmd = ['nix-env', '--uninstall']
        cmd.extend(self.selectors)
        rc, out, err = self.module.run_command(cmd)

        if rc != 0:
            msg = '{0} failed: {1}'.format(cmd, err)
            self.module.fail_json(msg=msg, rc=rc, stdout=out, stderr=err)

        packages_after = self._installed_packages()

        if len(packages_after) > 0:
            msg = '{0} failed: {1}'.format(cmd, err)
            self.module.fail_json(msg=msg, rc=rc, stdout=out, stderr=err)
        else:
            self.module.exit_json(changed=True, rc=rc, stdout=out, stderr=err)

    def run(self):

        if self.selectors:

            self._validate_selectors()

            if self.state == 'present':
                return self._install()
            elif self.state == 'latest':
                return self._upgrade()
            elif self.state == 'absent':
                return self._uninstall()

        self.module.exit_json(changed=False)

def main():
    module = AnsibleModule(
        argument_spec = dict(
            state = dict(default='present', choices=['present', 'absent', 'latest']),
            selector = dict(required=True, aliases=['name'], type='list'),
        ),
        supports_check_mode=True
    )
    p = module.params
    selectors = p['selector']
    state = p['state']
    try:
        nix = Nix(module, selectors=selectors, state=state)
        (success, retvals) = nix.run()
        if success:
            module.exit_json(**retvals)
        else:
            module.fail_json(**retvals)
    except NixException as e:
        module.fail_json(msg=str(e))

# import module snippets
from ansible.module_utils.basic import *

if __name__ == '__main__':
    main()
