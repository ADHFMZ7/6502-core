from cpu import CPU
from bus import BUS


def main():
    bus = BUS()
    cpu = CPU(bus) 
    
    cpu.reset() 

    bus.dump_memory(65000)
    # bus.dump_memory_better(65000)

    # while 1:
    #     cpu.clock()
    #     print(f"{hex(cpu.address)} {hex(cpu.data)} {bus.r}")
    #     if cpu.address == 0xffff:
    #         break
        
        
if __name__ == '__main__':
    main()
