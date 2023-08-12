from cpu import CPU
from bus import BUS


def main():
    bus = BUS()
    cpu = CPU(bus) 
         
    bus.load_rom("../nestest.nes", 0x8000) 
        
    #while cpu.address < 0xffff:
    for i in range(200):
        cpu.clock()
        
    print("CPU halted") 
    
if __name__ == '__main__':
    main()

