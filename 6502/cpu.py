C = 1  # Carry
Z = 2  # Zero
I = 4  # Interrupt Disable
D = 8  # Decimal Mode

B = 16 # Break Command
U = 32 # Unused
V = 64 # Overflow 
N = 128 # Negative

class CPU:

    def __init__(self):
        self.a = 0
        self.x = 0
        self.y = 0
        self.stkp = 0
        self.pc = 0
        self.status = 0
        self.data = 0

    def reset(self):
        pass 

    def irq(self):
        pass
    
    def nmi(self):
        pass
    
    def fetch(self):
        pass
    
    def clock(self):
        pass
    
    def read(self, address):
        pass
    
    def write(self, address, data):
        pass
    
    
    def set_flag(self, flag, val):
        if val:
            self.status |= flag
        else:
            self.status &= ~flag
      
    def get_flag(self, flag):
        return (self.status & flag) > 0  


    # Addressing Modes
    def IMP(self):
        pass
    
    def IMM(self):
        pass
    
    def ZP0(self):
        pass
    
    def ZPX(self):
        pass
    
    def ZPY(self):
        pass
    
    def REL(self):
        pass
    
    def ABS(self):
        pass
    
    def ABX(self):
        pass
    
    def ABY(self):
        pass
    
    def IND(self):
        pass
    
    def IZX(self):
        pass
    
    def IZY(self):
        pass
    

    # OP CODES
    def ADC(self):

        value = self.data + (self.get_flag(C) << 8)
        if value >= 256:
            self.set_flag(C, True)
        else:  
            self.set_flag(C, False)
            
        self.set_flag(Z, (value & 0x00FF) == 0)
        self.set_flag(N, value & 0x80)
        
        self.a = value & 0x00FF
        return 1

    def AND(self):
        self.a = self.a & self.data
        self.set_flag(Z, self.a == 0x00)
        self.set_flag(N, self.a & 0x80) 
        return 1 
        
    def ASL(self):
        pass

    def BCC(self):
        pass

    def BCS(self):
        pass

    def BEQ(self):
        pass

    def BIT(self):
        pass

    def BMI(self):
        pass

    def BNE(self):
        pass

    def BPL(self):
        pass

    def BRK(self):
        pass

    def BVC(self):
        pass

    def BVS(self):
        pass

    def CLC(self):
        self.set_flag(C, False)

    def CLD(self):
        self.set_flag(D, False)

    def CLI(self):
        self.set_flag(I, False)

    def CLV(self):
        self.set_flag(V, False)

    def CMP(self):
        pass

    def CPX(self):
        pass

    def CPY(self):
        pass

    def DEC(self):
        pass

    def DEX(self):
        pass

    def DEY(self):
        pass

    def EOR(self):
        pass

    def INC(self):
        pass

    def INX(self):
        pass

    def INY(self):
        pass

    def JMP(self):
        pass

    def JSR(self):
        pass

    def LDA(self):
        pass

    def LDX(self):
        pass

    def LDY(self):
        pass

    def LSR(self):
        pass

    def NOP(self):
        pass

    def ORA(self):
        pass

    def PHA(self):
        pass

    def PHP(self):
        pass

    def PLA(self):
        pass

    def PLP(self):
        pass

    def ROL(self):
        pass

    def ROR(self):
        pass

    def RTI(self):
        pass

    def RTS(self):
        pass

    def SBC(self):
        pass

    def SEC(self):
        pass

    def SED(self):
        pass

    def SEI(self):
        pass

    def STA(self):
        pass

    def STX(self):
        pass

    def STY(self):
        pass

    def TAX(self):
        pass

    def TAY(self):
        pass

    def TSX(self):
        pass

    def TXA(self):
        pass

    def TXS(self):
        pass

    def TYA(self):
        pass
