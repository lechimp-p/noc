import random
import string
from behave import *

def can_to_modifier(can):
    if can == "can":
        return id
    elif can == "can not":
        return (lambda x : not x)
    else:
        raise ValueError("Either you can or you can not, but i don't know how you '%s'" % can)

def random_string(length = 10):
    return ''.join(random.choice(string.ascii_uppercase + string.digits) for _ in range(0, length))

@given("User {user} exists")
def step_impl(context, user):
    print context.users.keys()
    if not user in context.users:
        try:
            context.users[user] = context.NoC.user(user, user)
        except context.NoC.NoCError:
            context.users[user] = context.NoC.user.create(context.users["admin"], user, user)

@when("I am {user}")
def step_impl(context, user):
    context.actor = context.users[user]

@then("I am {what}")
def step_impl(context, what):
    if what == "logged in":
        return True
    else:
        raise ValueError("Don't know how i could possibly be %s." % what)

@then("I {can} create a {what} \"{name}\"")
def step_impl(context, can, what, name):
    mod = can_to_modifier(can)
       
    if what == "channel":
        try:
            context.channels[name] = context.NoC.channel.create(context.actor, name, "Description of %s" % name)
            raised = False
        except context.NoC.NoCError:
            raised = True
        return mod(raised)
            
        
    elif what == "user":
        try: 
            context.users[name] = context.NoC.user.create(context.actor, name, name)
            raised = False
        except context.NoC.NoCError:
            raised = True
        return mod(raised)

    else:
        raise ValueError("Don't know how i could possibly create a %s" % what)

@then("I {can} set {property} of {what} \"{name}\"")
def step_imp(context, can, property, what, name):
    mod = can_to_modifier(can)

    if what == "channel":
        value = random_string()
        try:
            context.channels[name].set(context.actor, **{ property : value })
            raised = False
        except context.NoC.NoCError:
            raised = True
        return mod(raised) 
    
    elif what == "user": 
        if property == "icon":
            value = "icon.png"
        elif property in ["password", "login"]:
            value = name 
        else:
            value = random_string()

        try:
            context.users[name].set(context.actor, **{ property : value })
            raised = False
        except context.NoC.NoCError:
            raised = True
        return mod(raised) 

    else:
        raise ValueError("Don't know how i could possibly set something for a %s" % what)
