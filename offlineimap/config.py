
from subprocess import check_output
import string

def get_remotepass(pass_name):
    remotepass = check_output(['pass', 'show', pass_name], shell=False)
    remotepass = remotepass.rstrip('\n')
    if not remotepass:
        msg = "Cannot get remote password for {0}".format(pass_name)
        raise RuntimeError(msg)
    return remotepass


# There're 8 special folders on Gmail.
#
# - [Gmail]/All Mail
# - [Gmail]/Chats
# - [Gmail]/Drafts
# - [Gmail]/Important
# - [Gmail]/Sent Mail
# - [Gmail]/Spam
# - [Gmail]/Starred
# - [Gmail]/Trash

# Gnus doesn't accept "[]/" as pathname characters.  OfflineIMAP translates "/"
# to "." by default.  Here we need to translate "[]" to "{}".

def gmail_local_nametrans(name):
    return name.translate(string.maketrans('{}', '[]'))

def gmail_remote_nametrans(name):
    return name.translate(string.maketrans('[]', '{}'))
