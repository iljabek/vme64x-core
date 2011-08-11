import pyvmelib
import sys

class CVMeCrPos:
    def __init__(self, add, nbytes, name):
        self.name = name;
        self.nbytes = nbytes;
        self.add = add;
        self.value = [0] * nbytes;
        self.readdone = 0;
        self.writedone = 0;
        self.debug = 1;


    def read(self, map):
        for i in range(0,self.nbytes):
            vtemp= map.read(offset=self.add-3+i*4, width=32)[0];
            self.value[i] =vtemp;
            self.readdone=1+self.readdone;
        print "I am going to check debug mode"
        if self.debug == 1:
            print self.name;
            print self.readdone;
            print [ hex(x) for x in self.value ]
            print "I should have printed name and value"

    def write(self, map, data): 
        for i in range(0,self.nbytes):
            map.write(offset=self.add-3+i*4, width=32, values=data[i])
        self.writedone=1+self.writedone;

class CVmeCrList:
    def __init__(self,ga):
   #     par = self.parityOf(ga);
        self.gad = (ga <<19);
        self.size = 0x10000;
        self.data_width = 32;
        self.am = 0x2f;
	print '%x' % self.gad
        self.map = pyvmelib.Mapping(am=0x2f, base_address=self.gad, data_width=self.data_width, size=self.size);
	if self.map.vaddr is None:
		print "mapping failed!"
		sys.exit()
        self.cr = {"CHKSUMP": CVMeCrPos(0x03,1,"CHKSUM"), 
                   "CRDW":  CVMeCrPos(0x13,3,"CRDW"), 
                   "ACSRDW": CVMeCrPos(0x17,1,"ACSRDW"), 
                   "SPACEID": CVMeCrPos(0x1B,1,"SPACEID"),
                   "CASCII": CVMeCrPos(0x1F,1,"CASCII"),
                   "RASCII": CVMeCrPos(0x23,1,"RASCII"),
                   "MID": CVMeCrPos(0x27,3,"MID"),
                   "BID": CVMeCrPos(0x33,4,"BID"),
                   "RID": CVMeCrPos(0x43,4,"RID"),
                   "STRP": CVMeCrPos(0x53,3,"STRP"),
                   "PID": CVMeCrPos(0x7F,8,"PID"),
                   "FDAW0": CVMeCrPos(0x103,1,"FDAW0"),
                   "FDAW1": CVMeCrPos(0x107,1,"FDAW1"),
                   # "FDAW2": CVMeCrPos(0x10B,1,"FDAW2"),
                   "FDAW3": CVMeCrPos(0x10F,1,"FDAW3"),                  
                   "FDAW4": CVMeCrPos(0x103,1,"FDAW4"),
                   "FDAW5": CVMeCrPos(0x107,1,"FDAW5"),
                   # "FDAW6":  CVMeCrPos(0x10B,1,"FDAW6"),
                   "FDAW7": CVMeCrPos(0x10F,1,"FDAW7"), 
		   }

#    def parityOf(int_type):
#        parity = 0;
#        while (int_type):
#            parity = ~parity
#            int_type = int_type & (int_type - 1)
#            return(parity);
        
    def readCR(self):
        for s in self.cr:
	    print s
            self.cr[s].read(self.map);





##for i in range(16):
print "I am going to create modcr= CVmeCrList(6)"
modcr= CVmeCrList(6)
print "I am going to read modcr.readCR"
modcr.readCR()

##map = pyvmelib.Mapping(am=0x2f, base_address=0x300000, data_width=32, size=0x10000);


#for s in  
#value = modcr.map.read(offset=0x4, num=1, width=32)[0]
#print hex(value)

#map.write(offset=0x3, width=8, values=0xa5)
#map.write(offset=0x3, width=8, values=[0xa5, 0xff])
