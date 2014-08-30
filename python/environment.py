import NoC 
import subprocess
import shutil 
import os.path as path
import os
import time

def before_all(context):
    context.server_process = subprocess.Popen([path.abspath("../Server/dist/build/NoC-Server-dev/NoC-Server-dev")])
    time.sleep(1)

def after_all(context):
    context.server_process.terminate()
    context.server_process.wait()
    shutil.rmtree(path.abspath("./state"))   
    shutil.rmtree(path.abspath("./files"))   
    os.remove(path.abspath("./client_session_key.aes"))

def before_scenario(context, feature):
    context.users = {}
    context.channels = {}
    context.users["admin"] = NoC.user("admin", "admin")
    context.NoC = NoC
