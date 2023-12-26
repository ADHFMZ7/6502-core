import pygame, sys
from dataclasses import dataclass

from bus import BUS

OFFSET = 0x0200
SCREEN_X = 32
SCREEN_Y = 32

@dataclass
class Screen:
    x: int
    y: int
    screen: pygame.Surface
    square: pygame.Surface 

def init_screen(scr_x, scr_y):
    pygame.display.set_caption("Snake game")
    return Screen(scr_x, 
           scr_y, 
           pygame.display.set_mode((scr_x, scr_y)), 
           pygame.Surface((1, 1)))

def draw_screen(bus: BUS, screen: Screen):
    #screen.screen.fill((1, 1, 1))
    for j in range(screen.y):
        for i in range(screen.x):
            screen.square.fill(bus.read(0x0200 + (32 * j) + i))
            draw = pygame.Rect((j + 1), (i + 1), 1, 1)
            scaled_obj = pygame.transform.scale(screen.screen, (600, 600))
            screen.screen.blit(screen.square, draw)

