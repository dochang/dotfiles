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
    aliases: [ "selector" ]
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

    def _dry_run(self, args):
        cmd = ['nix-env', '--dry-run']
        cmd.extend(args)
        return self.module.run_command(cmd)

    def _run(self, args):
        cmd = ['nix-env']
        cmd.extend(args)
        return self.module.run_command(cmd)

    def _filter_output(self, output, pattern):
        if isinstance(output, basestring):
            lines = output.splitlines()
        else:
            lines = output
        pattern = re.compile(pattern)
        return [ line for line in lines if pattern.match(line) ]

    def _list_packages(self, available=False):

        args = ['--query']
        if available:
            args.append('--available')
        args.extend(self.selectors)
        rc, out, err = self._run(args)

        packages = set(out.splitlines())
        packages.discard('')

        return packages

    def _installed_packages(self):
        return self._list_packages(available=False)

    def _available_packages(self):
        return self._list_packages(available=True)

    def _install(self):

        args = ['--install']
        args.extend(self.selectors)

        rc, out, err = self._dry_run(args)

        installing = self._filter_output(err, '^installing ')
        replacing = self._filter_output(err, '^replacing old ')

        if len(installing) == len(replacing):
            self.module.exit_json(changed=False)

        if self.module.check_mode:
            self.module.exit_json(changed=True)

        rc, out, err = self._run(args)

        if rc != 0:
            msg = 'nix-env {0} failed: {1}'.format(args, err)
            self.module.fail_json(msg=msg, rc=rc, stdout=out, stderr=err)

        rc2, out2, err2 = self._dry_run(args)

        installing = self._filter_output(err2, '^installing ')
        replacing = self._filter_output(err2, '^replacing old ')

        if len(installing) != len(replacing):
            msg = 'nix-env {0} failed: {1}'.format(args, err)
            self.module.fail_json(msg=msg, rc=rc, stdout=out, stderr=err)
        else:
            self.module.exit_json(changed=True, rc=rc, stdout=out, stderr=err)

    def _upgrade(self):

        args = ['--upgrade']
        args.extend(self.selectors)

        rc, out, err = self._dry_run(args)

        upgrading = self._filter_output(err, '^upgrading ')
        downgrading = self._filter_output(err, '^downgrading ')

        if len(upgrading) + len(downgrading) == 0:
            self.module.exit_json(changed=False)

        if self.module.check_mode:
            self.module.exit_json(changed=True)

        rc, out, err = self._run(args)

        if rc != 0:
            msg = 'nix-env {0} failed: {1}'.format(args, err)
            self.module.fail_json(msg=msg, rc=rc, stdout=out, stderr=err)

        rc2, out2, err2 = self._dry_run(args)

        upgrading = self._filter_output(err2, '^upgrading ')
        downgrading = self._filter_output(err2, 'downgrading ')

        if len(upgrading) + len(downgrading) > 0:
            msg = 'nix-env {0} failed: {1}'.format(args, err)
            self.module.fail_json(msg=msg, rc=rc, stdout=out, stderr=err)
        else:
            self.module.exit_json(changed=True, rc=rc, stdout=out, stderr=err)

    def _uninstall(self):

        args = ['--uninstall']
        args.extend(self.selectors)

        rc, out, err = self._dry_run(args)

        uninstalling = self._filter_output(err, '^uninstalling ')

        if len(uninstalling) == 0:
            self.module.exit_json(changed=False)

        if self.module.check_mode:
            self.module.exit_json(changed=True)

        rc, out, err = self._run(args)

        if rc != 0:
            msg = '{0} failed: {1}'.format(args, err)
            self.module.fail_json(msg=msg, rc=rc, stdout=out, stderr=err)

        rc2, out2, err2 = self._dry_run(args)

        uninstalling = self._filter_output(err2, '^uninstalling ')

        if len(uninstalling) > 0:
            msg = 'nix-env {0} failed: {1}'.format(args, err)
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
