import os.path as path
import base64
import requests
import json
import pdb

base_url = "http://localhost:8000"
json_headers = {"content-type" : "application/json"}

class NoCError(Exception):
    def __init__(self, response):
        self.response = response
    def __str__(self):
        return "NoCError = %s" % self.response

class user(object):
    login_path = base_url + "/login"
    logout_path = base_url + "/logout"
    base_path = base_url + "/user"
    user_path = base_url + "/user/%i"    
    subscriptions_path = base_url + "/user/%i/subscriptions"
    channels_path = base_url + "/user/%i/channels"
    notifications_path = base_url + "/user/%i/notifications"

    # Creation

    def __init__(self, login = None, password = None, id = None):
        super(user, self).__init__()
        self.cookies = {}
        err = TypeError("Either set id or login and password")

        if not id is None:
            if not login is None and not password is None:
                raise err
            self.id = id
        else:
            if login is None or password is None:
                raise err
            self.id = self.postR(self.login_path, {"login":login, "password":password})["id"]

    @staticmethod
    def create(op, login, password):
        op.postR( user.base_path, { "login" : login, "password" : password })
        return user(login, password)         

    @staticmethod
    def search(op, login):
        print "HERE: " + login
        return op.getR( user.base_path, { "login" : login })

    # Methods

    def set(self, op, **dct):
        if "icon" in dct:
            dct["icon"] = image_json(dct["icon"])

        return op.postR(self.user_path % self.id, dct)

    def get(self, op):
        return op.getR(self.user_path % self.id)

    def getContacts(self, op):
        return op.getR(self.contacts_path % self.id)

    def modifyContacts(self, op, add = [], remove = []):
        return op.postR(self.contacts_path % self.id, {"add" : add, "remove" : remove})

    def getSubscriptions(self, op):
        return op.getR(self.contacts_path % self.id)

    def modifySubscriptions(self, op, subscribe = [], unsubscribe = []):
        return op.postR(self.contacts_path % self.id, {"subscribe" : subscribe, "unsubscribe" : unsubscribe})

    def getChannels(self, op):
        return op.getR(self.contacts_path % self.id)

    def getNotifications(self, op):
        return op.getR(self.notifications_path % self.id)

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
    base_path = base_url + "/channel"
    channel_path = base_url + "/channel/%i"    
    messages_path = base_url + "/channel/%i/messages"
    users_path = base_url + "/channel/%i/users"

    def __init__(self, id):
        super(channel, self).__init__()
        self.id = id

    @staticmethod
    def create(op, name, description):
        r = op.postR( channel.base_path, { "name" : name, "description" : description })
        return channel(r["id"])

    def set(self, op, **dct):
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
        
