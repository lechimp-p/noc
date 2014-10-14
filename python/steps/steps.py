import random
import string
from behave import *

def to_modifier(verb):
    if "not" in verb:
        return (lambda x : not x)
    else:
        return lambda x : x 

def check_verb(verb, word):
    if not word in ["%s not" % verb, "not %s" % verb, verb]:
        raise ValueError("Either you %s or you %s not, but i don't know how you '%s'" % (verb, verb, word))

def random_string(length = 10):
    return ''.join(random.choice(string.ascii_uppercase + string.digits) for _ in range(0, length))

@given("User {user} exists")
def step_impl(context, user):
    if not user in context.users:
        try:
            context.users[user] = context.NoC.user(user, user)
        except context.NoC.NoCError:
            context.users[user] = context.NoC.createUser(context.users["admin"], user, user)

@given("Channel {channel} exists")
def step_impl(context, channel):
    if not channel in context.channels:
        context.channels[channel] = context.NoC.createChannel(context.users["admin"], channel)

@when("I am {user}")
def step_impl(context, user):
    context.actor = context.users[user]

@then("I am {what}")
def step_impl(context, what):
    if what == "logged in":
        pass
    else:
        raise ValueError("Don't know how i could possibly be %s." % what)

@then("I {can} create a {what} \"{name}\"")
def step_impl(context, can, what, name):
    check_verb("can", can)
    mod = to_modifier(can)
       
    worked = None 
    if what == "channel":
        try:
            context.channels[name] = context.NoC.createChannel(context.actor, name)
            worked = True 
        except context.NoC.NoCError:
            worked = False 
        assert mod(worked)
        
    elif what == "user":
        try: 
            context.users[name] = context.NoC.createUser(context.actor, name, name)
            worked = True 
        except context.NoC.NoCError:
            worked = False 
        assert mod(worked)
    else:
        raise ValueError("Don't know how i could possibly create a %s" % what)


@when("I create a channel \"{name}\"")
def step_impl(context, name):
    create_channel(context.actor, name)

@when("{user} creates a channel \"{name}\"")
def step_impl(context, user, name):
    create_channel(context, context.users[user], name)

def create_channel(context, who, name):
    context.channels[name] = context.NoC.createChannel(who, name)

@then("I {can} set {property} of {what} \"{name}\"")
def step_imp(context, can, property, what, name):
    check_verb("can", can)
    mod = to_modifier(can)

    worked = None 
    if what == "channel":
        value = random_string()
        try:
            context.channels[name].set(context.actor, **{ property : value })
            worked = True 
        except context.NoC.NoCError:
            worked = False 
        assert mod(worked) 
    
    elif what == "user": 
        if property == "icon":
            value = "icon.png"
        elif property in ["password", "login"]:
            value = name 
        else:
            value = random_string()

        try:
            context.users[name].set(context.actor, **{ property : value })
            worked = True 
        except context.NoC.NoCError:
            worked = False 
        assert mod(worked) 

    else:
        raise ValueError("Don't know how i could possibly set something for a %s" % what)

@when("I search for {what} {search}") 
def step_impl(context, what, search):
    if what == "channel":
        res = context.NoC.searchChannel(context.actor, search)
        context.search_result = []
        for r in res["result"]:
            context.search_result.append(r["name"]) 
    elif what == "user":
        res = context.NoC.searchUser(context.actor, search)
        context.search_result = []
        for r in res["result"]:
            context.search_result.append(r["login"]) 
    else:
        raise ValueError("Don't know how to search for a %s" % what)

@then("the result will {contain} \"{name}\"")
def step_impl(context, contain, name):
    check_verb("contain", contain)
    mod = to_modifier(contain)
    assert mod(name in context.search_result)
