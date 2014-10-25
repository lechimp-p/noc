import os.path as path
import base64
import requests
import json
import pdb

json_headers = {"content-type" : "application/json"}

class NoCError(Exception):
    def __init__(self, response):
        self.response = response
    def __str__(self):
        return self.response

class noc(object):
    NoCError = NoCError

    def __init__(self, base_url):
        self.base_url = base_url
        self.user_base_path = self.base_url + "/user"
        self.channel_base_path = self.base_url + "/channel"

    def user(self, *args, **kwargs):
        return user(self, *args, **kwargs) 
    def channel(self, *args, **kwargs):
        return channel(self, *args, **kwargs)

    def createUser(self, op, login, password):
        op.postR( self.user_base_path, { "login" : login, "password" : password })
        return self.user(login, password)         

    def searchUser(self, op, login):
        return op.getR( self.user_base_path, { "login" : login })

    def createChannel(self, op, name):
        r = op.postR( self.channel_base_path, { "name" : name })
        return channel(self, r["id"])

    def searchChannel(self, op, name):
        return op.getR( self.channel_base_path, { "name" : name })



class user(object):
   # Creation

    def __init__(self, noc, login = None, password = None, id = None):
        super(user, self).__init__()
        self.cookies = {}
        err = TypeError("Either set id or login and password")

        self.login_path = noc.base_url + "/login"
        self.logout_path = noc.base_url + "/logout"
        self.login_info_path = noc.base_url + "/logininfo"
        self.base_path = noc.base_url + "/user"
        self.user_path = noc.base_url + "/user/%i"    
        self.subscriptions_path = noc.base_url + "/user/%i/subscriptions"
        self.contacts_path = noc.base_url + "/user/%i/contacts"
        self.notifications_path = noc.base_url + "/user/%i/notifications"

        self.noc = noc

 
        if not id is None:
            if not login is None and not password is None:
                raise err
            self.id = id
        else:
            if login is None or password is None:
                raise err
            self.id = self.postR(self.login_path, {"login":login, "password":password})["id"]

    # Methods

    def set(self, op, **dct):
        if "icon" in dct:
            dct["icon"] = image_json(dct["icon"])

        return op.postR(self.user_path % self.id, dct)

    def get(self, op):
        return op.getR(self.user_path % self.id)

    def getContacts(self, op):
        return op.getR(self.contacts_path % self.id)

    def setContacts(self, op, set = [], remove = []):
        def toContact(a):
            return { "userId" : a[0], "channelId" : a[1]}
        _set = map(toContact, set)
        return op.postR(self.contacts_path % self.id, {"set" : _set, "remove" : remove})

    def getSubscriptions(self, op):
        return op.getR(self.subscriptions_path % self.id)

    def setSubscriptions(self, op, subscribe = [], unsubscribe = []):
        return op.postR(self.subscriptions_path % self.id, {"subscribe" : subscribe, "unsubscribe" : unsubscribe})

    def getChannels(self, op):
        return op.getR(self.contacts_path % self.id)

    def getNotifications(self, op):
        return op.getR(self.notifications_path % self.id)

    def getLoginInfo(self):
        return self.getR(self.login_info_path)

    # Request handling

    def getR(self, path, params = {}):
        return self.get_postR(requests.get, path, None, params)

    def postR(self, path, data):
        return self.get_postR(requests.post, path, data, None)

    def get_postR(self, method, path, data, params):
        r = method( path
                  , headers = json_headers
                  , data = None if data is None else json.dumps(data)
                  , cookies = self.cookies
                  , params = params
                  ) 
        cookies = r.cookies.get_dict()
        #print str(r.status_code) + ": " + r.text
        if len(cookies) > 0:
            self.cookies = cookies
        if not 200 <= r.status_code < 300: 
            #pdb.set_trace();
            raise NoCError(r.text)
        if len(r.text) > 0:
            return r.json()
        else:
            return None
 

class channel(object):
    def __init__(self, noc, id):
        super(channel, self).__init__()
        self.id = id

        self.base_path = noc.base_url + "/channel"
        self.channel_path = noc.base_url + "/channel/%i"    
        self.messages_path = noc.base_url + "/channel/%i/messages"
        self.users_path = noc.base_url + "/channel/%i/users"

        self.noc = noc

    def set(self, op, **dct):
        if "image" in dct:
            dct["image"] = image_json(dct["image"])

        op.postR(self.channel_path % self.id, dct)

    def get(self, op):
        return op.getR(self.channel_path % self.id)

    def messages(self, op, offset, amount):
        return op.getR(self.messages_path % self.id, { "offset" : offset, "amount" : amount })

    def messagesTill(self, op, ts):
        return op.getR(self.messages_path % self.id, { "timestamp" : ts })

    def post(self, op, text, image = None):
        data = { "text" : text}
        if not image is None:
            data["image"] = image_json(image)
        return op.postR( self.messages_path % self.id, data )

    def modifyUsers(self, op, addOwners = [], removeOwners = [], addProducers = [], removeProducers = [], addConsumers = [], removeConsumers = []):
        return op.postR( self.users_path % self.id, { "addOwners" : addOwners, "removeOwners" : removeOwners, "addProducers" : addProducers, "removeProducers" : removeProducers, "addConsumers" : addConsumers, "removeConsumers" : removeConsumers })

def image_json(image):
    image = path.expanduser(image)
    if not path.exists(image):
        raise RuntimeError("Image '%s' does not exist." % image) 
    ext = path.splitext(image)[1].lower()
    if not ext in [".png", ".jpeg", ".jpg", ".gif"]:
        raise RuntimeError("Image '%s' is no png, jpeg or gif." % image)

    with open(image, "rb") as f:
        data = base64.b64encode(f.read())

    return { "type" : ext[1:], "data" : data }
        
