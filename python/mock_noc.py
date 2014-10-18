import sys
import NoC

noc = NoC.noc(sys.argv[1])
admin = noc.user("admin", "admin")
try:
    fjs = noc.createUser(admin, "fjs", "fjs")
except Exception as e:
    print e
    fjs = noc.user("fjs", "fjs") 
fjs.set(admin, name = "Franz Josef Strauss", description = "Bayrischer Koenig", icon="fjs.jpg", email="franz.josef@strauss.de")
admin.set(admin, icon="fjs.jpg")

chan0 = noc.createChannel(fjs, "Grillen in Koeln")
chan0.set(admin, description = "Wir wollen uns in Koeln zum grillen und chillen treffen.")
