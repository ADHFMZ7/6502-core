from cpu import CPU
from bus import BUS
from screen import init_screen, draw_screen, Screen
from snake import game_code

import pygame

def main():
    bus = BUS()
    cpu = CPU(bus) 
#    screen = init_screen(600, 600)

    
    # pygame.display.set_caption("Snake game")
    # screen = Screen(32, 
    #        32, 
    #        pygame.display.set_mode((32, 32)), 
    #        pygame.Surface((1, 1)))

    # bus.load_rom("tests/6502_functional_test.bin") 

    for i, byte in enumerate(game_code):
        bus.write(0x0600 + i, byte)
    bus.write(0xFFFC, 0x00)
    bus.write(0xFFFD, 0x06)
    cpu.reset()
    print("PC IS ", cpu.pc)

    while cpu.address < 0xffff:
    #for _ in range(20):
        #print(hex(cpu.pc))
        cpu.clock()
        #cpu.print_registers()
        #print(hex(cpu.pc))
        #bus.dump_memory_at_addr(cpu.pc)

        #draw_screen(bus, screen)
        for i in range(32):
            for j in range(32):
                print(bus.read(0x0200 + (i * 32) + j), end=" ")
            print()
        

    print("CPU halted") 
    
if __name__ == '__main__':
    main()

