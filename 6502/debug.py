from cpu import CPU
from bus import BUS

import curses



def main():
    bus = BUS()
    bus.load_rom("tests/6502_functional_test.bin", 0x0000) 

    cpu = CPU(bus) 
    # cpu.pc = 0x0600

    print("6502 Debugger")
    print("LOOK HERE", cpu.pc)    
    #bus.dump_memory_range(0x8000, 0x8100)
        
    bus.dump_memory_at_addr(cpu.pc)
    while 1:
        # print(f"{hex(cpu.address)} {hex(cpu.data)} {bus.r}")
        #bus.dump_memory_at_addr(cpu.pc)
        inp = input("\033[31m>>>\033[0m ")
        if inp: 
            command = inp
        else: 
            continue
        if command in ["h", "help"]:
            print_help()
        elif command in ["n", "next"]:
            cpu.clock()
            cpu.print_registers()
        elif command in ["p", "print"]:
            cpu.print_registers()
        elif command[0] in ["d", "display"]: 
            try:
                address = int(command.split()[1], 16)
                bus.dump_memory_at_addr(address)
                continue
            except:
                print("Invalid address")
        elif command in ["q", "quit"]:
            print("quitting debugger")
            exit(0)
        elif command in ["r", "reset"]:
            cpu.reset()
        elif command in ["s", "set"]:
            try:
                bus.memory[int(command.split()[1], 16)] = int(command.split()[2], 16)
            except:
                print("Invalid address or data")
        else:
            print("Invalid command")
            print("Type 'h' or 'help' for full list of commands")
             
        bus.dump_memory_at_addr(cpu.pc)

def print_help():
    print("h, help: print this help message")
    print("n, next: execute next instruction")
    print("p, print: print cpu state")
    print("d [address], display: display memory at address")
    print("q, quit: quit debugger")
    

if __name__ == '__main__':
    main()
