from cpu import CPU
from bus import BUS


def main():
    bus = BUS()
    cpu = CPU(bus) 
    
    cpu.reset() 

    bus.load_rom("s.bin", 0x8000) 
    #bus.dump_memory_range(0x8000, 0x8100)
    
    while 1:
        cpu.clock()
        # print(f"{hex(cpu.address)} {hex(cpu.data)} {bus.r}")

        bus.dump_memory_at_addr(cpu.pc)
        input()

        if cpu.address == 0xffff:
            break
        
        
if __name__ == '__main__':
    main()
