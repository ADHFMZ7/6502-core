from bus import BUS

# TODO: Stack ops and Interrupts
#       In the future; illegal ops

# C = 1  # Carry
# Z = 2  # Zero
# I = 4  # Interrupt Disable
# D = 8  # Decimal Mode
#
# B = 16 # Break Command
# U = 32 # Unused
# V = 64 # Overflow 
# N = 128 # Negative

C, Z, I, D, B, U, V, N = [2**i for i in range(8)]

class CPU:

    """
    This class represents the 6502 CPU. It connects to the bus via the data and address lines.
    """


    def __init__(self, bus: BUS):
        # Connected BUS device
        self.bus = bus 
      
    # def reset(self):
    #     # Load reset vector
    #     self.pc = self.read(0xFFFC) | (self.read(0xFFFD) << 8) 
    #     # Initialize registers
    #     self.a = 0 
    #     self.x = 0
    #     self.y = 0
    #     self.sp = 0xFD
    #     self.status = 0x00 | U 
       
        # Registers
        # self.a = 0
        # self.x = 0
        # self.y = 0
        # self.sp = 0
        # self.pc = 0 
        # self.status = 0 | U
        self.reset()

        # Internal Helpers 
        self.data = 0
        self.address = 0
       
        # Table of all ops
        self.lookup = {
                0:   (self.BRK, self.IMM, 7),   1: (self.ORA, self.IZX, 6),   2: (self.STP, self.IMP, 3),   3: (self.SLO, self.IZX, 8),   4: (self.NOP, self.ZP0, 3),   5: (self.ORA, self.ZP0, 3),   6: (self.ASL, self.ZP0, 5),   7: (self.SLO, self.ZP0, 5),   8: (self.PHP, self.IMP, 3),   9: (self.ORA, self.IMM, 2),  10: (self.ASL, self.ACC, 2),  11: (self.ANC, self.IMM, 2),  12: (self.NOP, self.ABS, 4),  13: (self.ORA, self.ABS, 4), 14: (self.ASL, self.ABS, 6), 15: (self.SLO, self.ABS, 6),
                16:  (self.BPL, self.REL, 2),  17: (self.ORA, self.IZY, 5),  18: (self.STP, self.IMP, 3),  19: (self.SLO, self.IZY, 8),  20: (self.NOP, self.ZPX, 4),  21: (self.ORA, self.ZPX, 4),  22: (self.ASL, self.ZPX, 6),  23: (self.SLO, self.ZPX, 6),  24: (self.CLC, self.IMP, 2),  25: (self.ORA, self.ABY, 4),  26: (self.NOP, self.IMP, 2),  27: (self.SLO, self.ABY, 7),  28: (self.NOP, self.ABX, 4),  29: (self.ORA, self.ABX, 4), 30: (self.ASL, self.ABX, 7), 31: (self.SLO, self.ABX, 7),
                32:  (self.JSR, self.ABS, 6),  33: (self.AND, self.IZX, 6),  34: (self.STP, self.IMP, 3),  35: (self.RLA, self.IZX, 8),  36: (self.BIT, self.ZP0, 3),  37: (self.AND, self.ZP0, 3),  38: (self.ROL, self.ZP0, 5),  39: (self.RLA, self.ZP0, 5),  40: (self.PLP, self.IMP, 4),  41: (self.AND, self.IMM, 2),  42: (self.ROL, self.ACC, 2),  43: (self.ANC, self.IMM, 2),  44: (self.BIT, self.ABS, 4),  45: (self.AND, self.ABS, 4), 46: (self.ROL, self.ABS, 6), 47: (self.RLA, self.ABS, 6),
                48:  (self.BMI, self.REL, 2),  49: (self.AND, self.IZY, 5),  50: (self.STP, self.IMP, 3),  51: (self.RLA, self.IZY, 8),  52: (self.NOP, self.ZPX, 4),  53: (self.AND, self.ZPX, 4),  54: (self.ROL, self.ZPX, 6),  55: (self.RLA, self.ZPX, 6),  56: (self.SEC, self.IMP, 2),  57: (self.AND, self.ABY, 4),  58: (self.NOP, self.IMP, 2),  59: (self.RLA, self.ABY, 7),  60: (self.NOP, self.ABX, 4),  61: (self.AND, self.ABX, 4), 62: (self.ROL, self.ABX, 7), 63: (self.RLA, self.ABX, 7),
                64:  (self.RTI, self.IMP, 6),  65: (self.EOR, self.IZX, 6),  66: (self.STP, self.IMP, 3),  67: (self.SRE, self.IZX, 8),  68: (self.NOP, self.ZP0, 3),  69: (self.EOR, self.ZP0, 3),  70: (self.LSR, self.ZP0, 5),  71: (self.SRE, self.ZP0, 5),  72: (self.PHA, self.IMP, 3),  73: (self.EOR, self.IMM, 2),  74: (self.LSR, self.ACC, 2),  75: (self.ALR, self.IMM, 2),  76: (self.JMP, self.ABS, 3),  77: (self.EOR, self.ABS, 4), 78: (self.LSR, self.ABS, 6), 79: (self.SRE, self.ABS, 6),
                80:  (self.BVC, self.REL, 2),  81: (self.EOR, self.IZY, 5),  82: (self.STP, self.IMP, 3),  83: (self.SRE, self.IZY, 8),  84: (self.NOP, self.ZPX, 4),  85: (self.EOR, self.ZPX, 4),  86: (self.LSR, self.ZPX, 6),  87: (self.SRE, self.ZPX, 6),  88: (self.CLI, self.IMP, 2),  89: (self.EOR, self.ABY, 4),  90: (self.NOP, self.IMP, 2),  91: (self.SRE, self.ABY, 7),  92: (self.NOP, self.ABX, 4),  93: (self.EOR, self.ABX, 4), 94: (self.LSR, self.ABX, 7), 95: (self.SRE, self.ABX, 7),
                96:  (self.RTS, self.IMP, 6),  97: (self.ADC, self.IZX, 6),  98: (self.STP, self.IMP, 3),  99: (self.RRA, self.IZX, 8),  100: (self.NOP, self.ZP0, 3), 101: (self.ADC, self.ZP0, 3), 102: (self.ROR, self.ZP0, 5), 103: (self.RRA, self.ZP0, 5), 104: (self.PLA, self.IMP, 4), 105: (self.ADC, self.IMM, 2), 106: (self.ROR, self.ACC, 2), 107: (self.ARR, self.IMM, 2), 108: (self.JMP, self.IND, 5), 109: (self.ADC, self.ABS, 4), 110: (self.ROR, self.ABS, 6), 111: (self.RRA, self.ABS, 6),
                112: (self.BVS, self.REL, 2), 113: (self.ADC, self.IZY, 5), 114: (self.STP, self.IMP, 3), 115: (self.RRA, self.IZY, 8), 116: (self.NOP, self.ZPX, 4), 117: (self.ADC, self.ZPX, 4), 118: (self.ROR, self.ZPX, 6), 119: (self.RRA, self.ZPX, 6), 120: (self.SEI, self.IMP, 2), 121: (self.ADC, self.ABY, 4), 122: (self.NOP, self.IMP, 2), 123: (self.RRA, self.ABY, 7), 124: (self.NOP, self.ABX, 4), 125: (self.ADC, self.ABX, 4), 126: (self.ROR, self.ABX, 7), 127: (self.RRA, self.ABX, 7),
                128: (self.NOP, self.IMM, 2), 129: (self.STA, self.IZX, 6), 130: (self.NOP, self.IMM, 2), 131: (self.SAX, self.IZX, 6), 132: (self.STY, self.ZP0, 3), 133: (self.STA, self.ZP0, 3), 134: (self.STX, self.ZP0, 3), 135: (self.SAX, self.ZP0, 3), 136: (self.DEY, self.IMP, 2), 137: (self.NOP, self.IMM, 2), 138: (self.TXA, self.IMP, 2), 139: (self.XAA, self.IMM, 2), 140: (self.STY, self.ABS, 4), 141: (self.STA, self.ABS, 4), 142: (self.STX, self.ABS, 4), 143: (self.SAX, self.ABS, 4),
                144: (self.BCC, self.REL, 2), 145: (self.STA, self.IZY, 6), 146: (self.STP, self.IMP, 3), 147: (self.AHX, self.IZY, 6), 148: (self.STY, self.ZPX, 4), 149: (self.STA, self.ZPX, 4), 150: (self.STX, self.ZPY, 4), 151: (self.SAX, self.ZPY, 4), 152: (self.TYA, self.IMP, 2), 153: (self.STA, self.ABY, 5), 154: (self.TXS, self.IMP, 2), 155: (self.TAS, self.ABY, 5), 156: (self.SHY, self.ABX, 5), 157: (self.STA, self.ABX, 5), 158: (self.SHX, self.ABY, 5), 159: (self.AHX, self.ABY, 5),
                160: (self.LDY, self.IMM, 2), 161: (self.LDA, self.IZX, 6), 162: (self.LDX, self.IMM, 2), 163: (self.LAX, self.IZX, 6), 164: (self.LDY, self.ZP0, 3), 165: (self.LDA, self.ZP0, 3), 166: (self.LDX, self.ZP0, 3), 167: (self.LAX, self.ZP0, 3), 168: (self.TAY, self.IMP, 2), 169: (self.LDA, self.IMM, 2), 170: (self.TAX, self.IMP, 2), 171: (self.LAX, self.IMM, 2), 172: (self.LDY, self.ABS, 4), 173: (self.LDA, self.ABS, 4), 174: (self.LDX, self.ABS, 4), 175: (self.LAX, self.ABS, 4),
                176: (self.BCS, self.REL, 2), 177: (self.LDA, self.IZY, 5), 178: (self.STP, self.IMP, 3), 179: (self.LAX, self.IZY, 5), 180: (self.LDY, self.ZPX, 4), 181: (self.LDA, self.ZPX, 4), 182: (self.LDX, self.ZPY, 4), 183: (self.LAX, self.ZPY, 4), 184: (self.CLV, self.IMP, 2), 185: (self.LDA, self.ABY, 4), 186: (self.TSX, self.IMP, 2), 187: (self.LAS, self.ABY, 4), 188: (self.LDY, self.ABX, 4), 189: (self.LDA, self.ABX, 4), 190: (self.LDX, self.ABY, 4), 191: (self.LAX, self.ABY, 4),
                192: (self.CPY, self.IMM, 2), 193: (self.CMP, self.IZX, 6), 194: (self.NOP, self.IMM, 2), 195: (self.DCP, self.IZX, 8), 196: (self.CPY, self.ZP0, 3), 197: (self.CMP, self.ZP0, 3), 198: (self.DEC, self.ZP0, 5), 199: (self.DCP, self.ZP0, 5), 200: (self.INY, self.IMP, 2), 201: (self.CMP, self.IMM, 2), 202: (self.DEX, self.IMP, 2), 203: (self.AXS, self.IMM, 2), 204: (self.CPY, self.ABS, 4), 205: (self.CMP, self.ABS, 4), 206: (self.DEC, self.ABS, 6), 207: (self.DCP, self.ABS, 6),
                208: (self.BNE, self.REL, 2), 209: (self.CMP, self.IZY, 5), 210: (self.STP, self.IMP, 3), 211: (self.DCP, self.IZY, 8), 212: (self.NOP, self.ZPX, 4), 213: (self.CMP, self.ZPX, 4), 214: (self.DEC, self.ZPX, 6), 215: (self.DCP, self.ZPX, 6), 216: (self.CLD, self.IMP, 2), 217: (self.CMP, self.ABY, 4), 218: (self.NOP, self.IMP, 2), 219: (self.DCP, self.ABY, 7), 220: (self.NOP, self.ABX, 4), 221: (self.CMP, self.ABX, 4), 222: (self.DEC, self.ABX, 7), 223: (self.DCP, self.ABX, 7),
                224: (self.CPX, self.IMM, 2), 225: (self.SBC, self.IZX, 6), 226: (self.NOP, self.IMM, 2), 227: (self.ISC, self.IZX, 8), 228: (self.CPX, self.ZP0, 3), 229: (self.SBC, self.ZP0, 3), 230: (self.INC, self.ZP0, 5), 231: (self.ISC, self.ZP0, 5), 232: (self.INX, self.IMP, 2), 233: (self.SBC, self.IMM, 2), 234: (self.NOP, self.IMP, 2), 235: (self.SBC, self.IMM, 2), 236: (self.CPX, self.ABS, 4), 237: (self.SBC, self.ABS, 4), 238: (self.INC, self.ABS, 6), 239: (self.ISC, self.ABS, 6),
                240: (self.BEQ, self.REL, 2), 241: (self.SBC, self.IZY, 5), 242: (self.STP, self.IMP, 3), 243: (self.ISC, self.IZY, 8), 244: (self.NOP, self.ZPX, 4), 245: (self.SBC, self.ZPX, 4), 246: (self.INC, self.ZPX, 6), 247: (self.ISC, self.ZPX, 6), 248: (self.SED, self.IMP, 2), 249: (self.SBC, self.ABY, 4), 250: (self.NOP, self.IMP, 2), 251: (self.ISC, self.ABY, 7), 252: (self.NOP, self.ABX, 4), 253: (self.SBC, self.ABX, 4), 254: (self.INC, self.ABX, 7), 255: (self.ISC, self.ABX, 7)
                }

    def clock(self):
        """
        Advances the CPU by one clock cycle. 
        
        This is the main function of the CPU. It fetches the next instruction,
        executes it and decrements the number of cycles left for the current 
        instruction. 
        """
        
        op, mode, cycles = self.fetch()
        self.execute(op, mode, cycles)
        return

    def fetch(self):
        """
        Fetches the next instruction from memory and returns the opcode, 
        addressing mode and base cycles.
        """
        self.address = self.pc 
        return self.lookup[self.read(self.pc)]

    def execute(self, op: callable, mode: callable, cycles: int):
        """
        executes the functions for the given opcode and addressing mode
        and increments the program counter. 
        
        It also calculates the number of cycles for the current instruction
        and runs the corresponding number of cycles.
        """
        start_addr = self.pc
        self.pc += 1
        mode_cycles = mode()  
        if mode == self.ACC:
            op_cycles = op(True)
        else:
            op_cycles = op()
     
        total_cycles = mode_cycles + op_cycles + cycles 
        if op_cycles and mode_cycles:
            if self.page_boundary_crossed(start_addr, self.pc - 1):
                total_cycles += 1
            if mode_cycles == 2: # branch taken
                total_cycles += 1

        # print(f"operation {op} will take {total_cycles} cycles using addressing mode {mode}")
        # for cycle in reversed(range(total_cycles)):
        #     print(f"{cycle} cycles left")

    def page_boundary_crossed(self, addr1: int, addr2: int):
        return (addr1 & 0xFF00) != (addr2 & 0xFF00)

    def reset(self):
        # Load reset vector
        self.pc = self.read(0xFFFC) | (self.read(0xFFFD) << 8) 
        # Initialize registers
        self.a = 0 
        self.x = 0
        self.y = 0
        self.sp = 0xFD
        self.status = 0x00 | U

    def irq(self):
        cycles = 7
        self.write(0x0100 + self.sp, (self.pc >> 8) & 0x00FF)
        self.sp -= 1 
       
        # TODO: FINISH THIS  
        
        return 0
    
    def nmi(self):
        return 0
    
    def read(self, address: int):
        return self.bus.read(address)
    
    def write(self, address: int, data: int):
        self.bus.write(address, data)
    
    def set_flag(self, flag: int, val: int):
        if val:
            self.status |= flag
        else:
            self.status &= ~flag
      
    def get_flag(self, flag: int):
        return (self.status & flag) > 0  


    # Addressing Modes
    # These addressing mode functions place the data that will be
    # operated on in the self.data variable.
    
    # The functions return how many possible extra cycles the instruction
    # could take.  
    
    def IMP(self): # Implied
        return 0
    
    def IMM(self): # Immediate
        self.address = self.pc
        self.pc += 1 
        
        self.data = self.read(self.address)
        return 0
    
    def ZP0(self): # Zero Page
        self.address = self.read(self.pc)
        self.data = self.read(self.address) 
        self.pc += 1 
        return 0 
    
    def ZPX(self): # Zero Page X
        self.address = (self.read(self.pc) + self.x) % 0x0100
        self.data = self.read(self.address) 
        self.pc += 1 
        return 0
    
    def ZPY(self): # Zero Page Y
        self.address = (self.read(self.pc) + self.y) % 0x0100
        self.data = self.read(self.address) 
        self.pc += 1 
        return 0
    
    def REL(self): # Relative
        """
        Calculates the relative jump that would be taken and put
        it into the address variable
        """
        self.address = self.read(self.pc) + self.pc
        self.data = self.read(self.address)
        self.pc += 1
        
        return 1
   
    def ABS(self): # Absolute
        """
        Gets the next two bytes and combines them into the address
        variable
        
        Also stores the data in the calculated memory address
        """
        self.address = self.read(self.pc) | (self.read(self.pc + 1) << 8)
        self.data = self.read(self.address)
        self.pc += 2
        return 0
    
    def ABX(self): # Absolute X THIS ONE IS MORE COMPLICATED CLOCKS
        self.address = self.read(self.pc) | (self.read(self.pc + 1) << 8) + self.x
        self.data = self.read(self.address)
        self.pc += 2
        return 1
    
    def ABY(self): # Absolute Y
        self.address = self.read(self.pc) | (self.read(self.pc + 1) << 8) + self.y
        self.data = self.read(self.address)
        self.pc += 2
        return 1
   
    def ACC(self): # Accumulator
        self.data = self.a
        return 0
    
    def IND(self): # Indirect 
        
        return 0

    def IZX(self): # Indexed Indirect X
        self.address
        return 0
    
    def IZY(self): # Indirect Indexed Y
        return 1
   

    # OP CODES
    # These functions are the actual instructions that the CPU executes.
   
    # These functions assume that the data to be operated on is in the 
    # self.data variable.
    
    # They return if the instruction may need an extra cycle to complete.
    # This along with the return value of the addressing mode function
    # determines how many cycles to add to the total cycle count.

    
    def ADC(self): # Add with Carry

        value = self.data + (self.get_flag(C) << 8)
        if value >= 256:
            self.set_flag(C, True)
        else:  
            self.set_flag(C, False)
            
        self.set_flag(Z, (value & 0x00FF) == 0)
        self.set_flag(N, value & 0x80)
        
        self.a = value & 0x00FF
        return 1

    def AND(self): # Logical AND
        self.a = self.a & self.data
        self.set_flag(Z, self.a == 0x00)
        self.set_flag(N, self.a & 0x80) 
        return 1 
        
    def ASL(self, is_acc: bool = False): # Arithmetic Shift Left
      
        self.set_flag(C, self.data & 0x80)
        val = self.data << 1
        self.set_flag(N, val & 0x80)
        self.set_flag(Z, val == 0x00)
        if is_acc:
            self.a = val
        else: 
            self.write(self.address, val)
        return 0

    def BCC(self): # Branch if Carry Clear
        
        if not self.get_flag(C):
            pc = self.address
            return 2
         
        return 1

    def BCS(self): # Branch if Carry Set
       
        if self.get_flag(C):
            pc = self.address
            return 2
        
        return 1

    def BEQ(self): # Branch if Equal
        
        if self.get_flag(Z):
            pc = self.address
            return 2
        
        return 1 

    def BIT(self): # Bit Test
       
        #TODO:  FINISH THIS   
        
        # self.set_flag(N)
        # self.set_flag(V)
        # self.set_flag(Z)
        return 0

    def BMI(self): # Branch if Minus
        
        if self.get_flag(N):
            self.pc = self.address
            return 2
        
        return 1

    def BNE(self): # Branch if Not Equal
        
        if self.get_flag(Z):
            return 1
        
        self.pc = self.address 
        return 2

    def BPL(self): # Branch if Positive
        
        if not self.get_flag(N):
            self.pc = self.address
            return 2 
        
        return 1

    def BRK(self): # Force Interrupt
        return 0

    def BVC(self): # Branch if Overflow Clear
        
        if not self.get_flag(V):
            self.pc = self.address
            return 2
        
        return 1

    def BVS(self): # Branch if Overflow Set
        
        if self.get_flag(V):
            self.pc = self.address
            return 2
        
        return 1

    def CLC(self): # Clear Carry Flag
        self.set_flag(C, False)
        return 0

    def CLD(self): # Clear Decimal Mode
        self.set_flag(D, False)
        return 0

    def CLI(self): # Clear Interrupt Disable
        self.set_flag(I, False)
        return 0

    def CLV(self): # Clear Overflow Flag
        self.set_flag(V, False)
        return 0

    def CMP(self): # Compare
        val = self.a - self.data
       
        self.set_flag(Z, val == 0)
        self.set_flag(N, val < 0)
        self.set_flag(C, val >= 0)
        return 1

    def CPX(self): # Compare X Register
        val = self.x - self.data
        
        self.set_flag(Z, val == 0)
        self.set_flag(N, val < 0)
        self.set_flag(C, val >= 0)
        return 0

    def CPY(self): # Compare Y Register
        val = self.y - self.data
        
        self.set_flag(Z, val == 0)
        self.set_flag(N, val < 0)
        self.set_flag(C, val >= 0)
        return 0

    def DEC(self): # Decrement Memory
        self.write(self.address, self.read(self.address) - 1)
        val = self.read(self.address)
        
        self.set_flag(Z, val == 0)
        self.set_flag(N, val < 0)
        return 0

    def DEX(self): # Decrement X Register
        self.x -= 1
        
        self.set_flag(Z, self.x == 0)
        self.set_flag(N, self.x < 0)
        return 0

    def DEY(self): # Decrement Y Register
        self.y -= 1
        
        self.set_flag(Z, self.y == 0)
        self.set_flag(N, self.y < 0)
        return 0

    def EOR(self): # Exclusive OR
        self.a = self.a ^ self.data
        
        self.set_flag(Z, self.a == 0)
        self.set_flag(N, self.a < 0)
        return 1

    def INC(self): # Increment Memory
        self.write(self.address, self.read(self.address) + 1)
        val = self.read(self.address)
        
        self.set_flag(Z, val == 0)
        self.set_flag(N, val < 0)
        return 0

    def INX(self): # Increment X Register
        
        self.x += 1
        
        self.set_flag(Z, self.x == 0)
        self.set_flag(N, self.x < 0)
        return 0

    def INY(self): # Increment Y Register
        self.y += 1
        
        self.set_flag(Z, self.y == 0)
        self.set_flag(N, self.y < 0)
        return 0

    def JMP(self): # Jump
        # TODO: Double check this is correct 
        self.pc = self.address 
      
        return 0

    def JSR(self): # Jump to Subroutine
        # TODO: THIS
        return 0

    def LDA(self): # Load Accumulator
        self.a = self.read(self.address)
        self.set_flag(Z, self.a == 0)
        self.set_flag(N, self.a < 0)
        return 1

    def LDX(self): # Load X Register
        self.x = self.read(self.address)
        self.set_flag(Z, self.x == 0)
        self.set_flag(N, self.x < 0)
        return 1

    def LDY(self): # Load Y Register
        self.y = self.read(self.address)
        self.set_flag(Z, self.y == 0)
        self.set_flag(N, self.y < 0)
        return 1

    def LSR(self, is_acc: bool = False): # Logical Shift Right
        self.set_flag(C, self.data & 0x01)
        val = self.data >> 1
        self.set_flag(N, 0)
        self.set_flag(Z, val == 0x00)
        if is_acc:
            self.a = val
        else: 
            self.write(self.address, val)
        return 0

    def NOP(self): # No Operation
        return 0

    def ORA(self): # Logical Inclusive OR
        self.a |= self.data
        self.set_flag(Z, self.a == 0)
        self.set_flag(N, self.a < 0)
        return 1

    def PHA(self): # Push Accumulator
        self.write(0x0100 + self.sp, self.a)
        self.sp -= 1
        return 0

    def PHP(self): # Push Processor Status
        self.write(0x0100 + self.sp, self.status)
        self.sp -= 1
        return 0

    def PLA(self): # Pull Accumulator
        self.sp += 1
        self.a = self.read(0x0100 + self.sp)
        self.set_flag(Z, self.a == 0)
        self.set_flag(N, self.a < 0)
        return 0

    def PLP(self): # Pull Processor Status
        self.sp += 1
        self.status = self.read(0x0100 + self.sp)
        return 0

    def ROL(self, is_acc: bool = False): # Rotate Left
        return 0

    def ROR(self, is_acc: bool = False): # Rotate Right
        return 0

    def RTI(self): # Return from Interrupt
        return 0

    def RTS(self): # Return from Subroutine
        return 0

    def SBC(self): # Subtract with Carry
    
        return 1

    def SEC(self): # Set Carry Flag
        self.set_flag(C, 1)
        return 0

    def SED(self): # Set Decimal Flag
        self.set_flag(D, 1)
        return 0

    def SEI(self): # Set Interrupt Disable
        self.set_flag(I, 1)
        return 0

    def STA(self): # Store Accumulator
        self.write(self.address, self.a)
        return 0

    def STX(self): # Store X Register
        self.write(self.address, self.x)
        return 0

    def STY(self): # Store Y Register
        self.write(self.address, self.y)
        return 0

    def TAX(self): # Transfer Accumulator to X
        self.x = self.a
        self.set_flag(N, self.x < 0)
        self.set_flag(Z, self.x == 0)
        return 0

    def TAY(self): # Transfer Accumulator to Y
        self.y = self.a
        self.set_flag(N, self.y < 0)
        self.set_flag(Z, self.y == 0)
        return 0

    def TSX(self): # Transfer Stack Pointer to X
        self.x = self.sp
        self.set_flag(N, self.x < 0)
        self.set_flag(Z, self.x == 0)
        return 0

    def TXA(self): # Transfer X to Accumulator
        self.a = self.x
        self.set_flag(N, self.a < 0)
        self.set_flag(Z, self.a == 0)
        return 0

    def TXS(self): # Transfer X to Stack Pointer
        self.sp = self.x
        return 0

    def TYA(self): # Transfer Y to Accumulator
        self.x = self.a
        self.set_flag(N, self.x < 0)
        self.set_flag(Z, self.x == 0)
        return 0

    # Illegal opcodes 
    def STP(self):
        return 0
    
    def SLO(self):
        return 0
    
    def ANC(self):
        return 0
    
    def RLA(self):
        return 0
    
    def SRE(self):
        return 0
    
    def RRA(self):
        return 0
    
    def SAX(self):
        return 0
    
    def LAX(self):
        return 0
    
    def ALR(self):
        return 0     
    
    def ARR(self):
        return 0
    
    def XAA(self):
        return 0
    
    def AHX(self):
        return 0
    
    def TAS(self):
        return 0
    
    def SHY(self):
        return 0
    
    def SHX(self):
        return 0
    
    def LAS(self):
        return 0
    
    def DCP(self):
        return 0
    
    def AXS(self):
        return 0
    
    def ISC(self):
        return 0
   
    def KIL(self):
        return 0
    
    def print_registers(self):
        print(" A: {:04x}".format(self.a), end="\t")
        print(" X: {:04x}".format(self.x), end="\t")
        print(" Y: {:04x}".format(self.y))
        print("PC: {:04x}".format(self.pc), end="\t")
        print("SP: {:04x}".format(self.sp), end="\t")

        print("Status:", end=" ")
        print(*(int(self.get_flag(2**i)) for i in range(8)), end="\n", sep="")
        
        
            
    
    
