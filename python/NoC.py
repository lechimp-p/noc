import requests
import json

base_url = "http://localhost:8000"
json_headers = {"content-type" : "application/json"}

class user(object):
    login_path = base_url + "/login"
    logout_path = base_url + "/logout"
    user_path = base_url + "/user/%i"    
    subscriptions_path = base_url + "/user/%i/subscriptions"
    channels_path = base_url + "/user/%i/channels"
    notifications_path = base_url + "/user/%i/notifications"

    def __init__(self, login, password):
        super(user, self).__init__()
        self.cookies = {}
        self.id = self.postR(self.login_path, {"login":login, "password":password})["id"]

    def getR(self, path, data = {}):
        return self.get_postR(requests.get, path, data)

    def postR(self, path, data):
        return self.get_postR(requests.post, path, data)

    def get_postR(self, method, path, data):
        r = method( path, headers = json_headers, data = json.dumps(data), cookies = self.cookies) 
        cookies = r.cookies.get_dict()
        if len(cookies) > 0:
            self.cookies = cookies
        print r.text
        r.raise_for_status()
        return r.json()

    @staticmethod
    def create(login, password):
        pass

    def set(self, op, **dct):
        return op.postR(self.user_path % self.id, dct)

    def get(self, op):
        return op.getR(self.user_path % self.id)

    def getContacts(self, op):
        return op.getR(self.contacts_path % self.id)

    def modifyContacts(self, op, add, remove):
        return op.postR(self.contacts_path % self.id, {"add" : add, "remove" : remove})

    def getSubscriptions(self, op):
        return op.getR(self.contacts_path % self.id)

    def modifySubscriptions(self, op, subscribe, unsubscribe):
        return op.postR(self.contacts_path % self.id, {"subscribe" : subscribe, "unsubscribe" : unsubscribe})

    def getChannels(self, op):
        return op.getR(self.contacts_path % self.id)
