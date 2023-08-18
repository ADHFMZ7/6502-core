from cpu import CPU
from bus import BUS


def main():
    bus = BUS(reset_vector = 0x8000)
    cpu = CPU(bus) 
    print("IT STARTS AT: ", cpu.pc) 
    bus.load_rom("../nestest.nes", 0x8000) 
    # while cpu.address < 0xffff:
    for _ in range(20):
        cpu.clock()
        
    print("CPU halted") 
    
if __name__ == '__main__':
    main()

