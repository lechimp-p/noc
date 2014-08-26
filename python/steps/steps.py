from behave import *

@given("User {user} exists")
def step_impl(context, user):
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
    if can == "can":
        mod = id
    elif can == "can not":
        mod = lambda x : not x
    else:
        raise ValueError("Either you can or you can not, but i don't know how you '%s'" % can)
        
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
