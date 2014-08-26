from behave import *

@given("User {user} exists")
def step_impl(context, user):
    if not user in context.users:
        try:
            context.users[user] = context.NoC.user(user, user)
        except NoCError:
            context.users[user] = context.NoC.user.create(self.context.users["admin"], user, user)

@when("I am {user}")
def step_impl(context, user):
    context.actor = context.users[user]

@then("I am {what}")
def step_impl(context, what):
    if what == "logged in":
        return True
    else:
        raise ValueError("Don't know how i could possibly be %s." % what)

@then("I can create a {what} \"{name}\"")
def step_impl(context, what, name):
    if what == "channel":
        context.channels[name] = context.NoC.channel.create(context.actor, name, "Description of %s" % name)
    elif what == "user":
        context.users[name] = context.NoC.user.create(context.actor, name, name)
    else:
        raise ValueError("Don't know how i could possibly create a %s" % what)
