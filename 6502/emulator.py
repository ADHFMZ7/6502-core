from cpu import CPU
from bus import BUS


def main():
    bus = BUS()
    cpu = CPU(bus) 
     
    cpu.reset() 
         
    bus.load_rom("s.bin", 0x8000) 
        
    while cpu.address < 0xffff:
        cpu.clock()
        
    print("CPU halted") 
    
if __name__ == '__main__':
    main()

