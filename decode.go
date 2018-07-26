package mips

import (
	"encoding/binary"
	"fmt"
	"log"

	"github.com/pkg/errors"
)

// Instruction encodings
//
//    R-type   Register       000000ss sssttttt dddddaaa aaffffff
//    I-type   Immediate      ooooooss sssttttt iiiiiiii iiiiiiii
//    J-type   Jump           ooooooii iiiiiiii iiiiiiii iiiiiiii
//    x-type   Co-processor   ooooooxx xxxxxxxx xxxxxxxx xxxxxxxx

// Instructions.
//
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | OP      | Description                                        | Encoding | Syntax                    |
//    +=========+====================================================+==========+===========================+
//    |         | Load/Store Instructions                                                                   |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | LB      | Load Byte                                          | I-type   | LB      rt, offset(base)  |
//    | LBU     | Load Byte Unsigned                                 | I-type   | LBU     rt, offset(base)  |
//    | LH      | Load Halfword                                      | I-type   | LH      rt, offset(base)  |
//    | LHU     | Load Halfword Unsigned                             | I-type   | LHU     rt, offset(base)  |
//    | LW      | Load Word                                          | I-type   | LW      rt, offset(base)  |
//    | LWL     | Load Word Left                                     | I-type   | LWL     rt, offset(base)  |
//    | LWR     | Load Word Right                                    | I-type   | LWR     rt, offset(base)  |
//    | SB      | Store Byte                                         | I-type   | SB      rt, offset(base)  |
//    | SH      | Store Halfword                                     | I-type   | SH      rt, offset(base)  |
//    | SW      | Store Word                                         | I-type   | SW      rt, offset(base)  |
//    | SWL     | Store Word Left                                    | I-type   | SWL     rt, offset(base)  |
//    | SWR     | Store Word Right                                   | I-type   | SWR     rt, offset(base)  |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    |         | Arithmetic Instructions (ALU Immediate)                                                   |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | ADDI    | Add Immediate                                      | I-type   | ADDI    rt, rs, immediate |
//    | ADDIU   | Add Immediate Unsigned                             | I-type   | ADDIU   rt, rs, immediate |
//    | SLTI    | Set on Less Than Immediate                         | I-type   | SLTI    rt, rs, immediate |
//    | SLTIU   | Set on Less Than Immediate Unsigned                | I-type   | SLTIU   rt, rs, immediate |
//    | ANDI    | AND Immediate                                      | I-type   | ANDI    rt, rs, immediate |
//    | ORI     | OR Immediate                                       | I-type   | ORI     rt, rs, immediate |
//    | XORI    | Exclusive OR Immediate                             | I-type   | XORI    rt, rs, immediate |
//    | LUI     | Load Upper Immediate                               | I-type   | LUI     rt, immediate     |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    |         | Arithmetic Instructions (3-operand, register-type)                                        |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | ADD     | Add                                                | R-type   | ADD     rd, rs, rt        |
//    | ADDU    | Add Unsigned                                       | R-type   | ADDU    rd, rs, rt        |
//    | SUB     | Subtract                                           | R-type   | SUB     rd, rs, rt        |
//    | SUBU    | Subtract Unsigned                                  | R-type   | SUBU    rd, rs, rt        |
//    | SLT     | Set on Less Than                                   | R-type   | SLT     rd, rs, rt        |
//    | SLTU    | Set on Less Than Unsigned                          | R-type   | SLTU    rd, rs, rt        |
//    | AND     | AND                                                | R-type   | AND     rd, rs, rt        |
//    | OR      | OR                                                 | R-type   | OR      rd, rs, rt        |
//    | XOR     | Exclusive OR                                       | R-type   | XOR     rd, rs, rt        |
//    | NOR     | NOR                                                | R-type   | NOR     rd, rs, rt        |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    |         | Shift Instructions                                                                        |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | SLL     | Shift Left Logical                                 | R-type   | SLL     rd, rt, shift     |
//    | SRL     | Shift Right Logical                                | R-type   | SRL     rd, rt, shift     |
//    | SRA     | Shift Right Arithmetic                             | R-type   | SRA     rd, rt, shift     |
//    | SLLV    | Shift Left Logical Variable                        | R-type   | SLLV    rd, rt, rs        |
//    | SRLV    | Shift Right Logical Variable                       | R-type   | SRLV    rd, rt, rs        |
//    | SRAV    | Shift Right Arithmetic Variable                    | R-type   | SRAV    rd, rt, rs        |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    |         | Multiply/Divide Instructions                                                              |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | MULT    | Multiply                                           | R-type   | MULT    rs, rt            |
//    | MULTU   | Multiply Unsigned                                  | R-type   | MULTU   rs, rt            |
//    | DIV     | Divide                                             | R-type   | DIV     rs, rt            |
//    | DIVU    | Divide Unsigned                                    | R-type   | DIVU    rs, rt            |
//    | MFHI    | Move From HI                                       | R-type   | MFHI    rd                |
//    | MTHI    | Move To HI                                         | R-type   | MTHI    rd                |
//    | MFLO    | Move From LO                                       | R-type   | MFLO    rd                |
//    | MTLO    | Move To LO                                         | R-type   | MTLO    rd                |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    |         | Jump and Branch Instructions                                                              |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | J       | Jump                                               | J-type   | J       target            |
//    | JAL     | Jump and Link                                      | J-type   | JAL     target            |
//    | JR      | Jump to Register                                   | R-type   | JR      rs                |
//    | JALR    | Jump and Link Register                             | R-type   | JALR    rs, rd            |
//    | BEQ     | Branch on Equal                                    | I-type   | BEQ     rs, rt, offset    |
//    | BNE     | Branch on Not Equal                                | I-type   | BNE     rs, rt, offset    |
//    | BLEZ    | Branch on Less than or Equal to Zero               | I-type   | BLEZ    rs, offset        |
//    | BGTZ    | Branch on Greater Than Zero                        | I-type   | BGTZ    rs, offset        |
//    | BLTZ    | Branch on Less Than Zero                           | I-type   | BLTZ    rs, offset        |
//    | BGEZ    | Branch on Greater Than or Equal to Zero            | I-type   | BGEZ    rs, offset        |
//    | BLTZAL  | Branch on Less Than Zero and Link                  | I-type   | BLTZAL  rs, offset        |
//    | BGEZAL  | Branch on Greater Than or Equal to Zero and Link   | I-type   | BGEZAL  rs, offset        |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    |         | Special Instructions                                                                      |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | SYSCALL | System Call                                        | R-type   | SYSCALL                   |
//    | BREAK   | Break                                              | R-type   | BREAK                     |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    |         | Coprocessor Instructions                                                                  |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | LWCz    | Load Word from Coprocessor                         | I-type   | LWCz     rt, offset(base) |
//    | SWCz    | Store Word to Coprocessor                          | I-type   | SWCz     rt, offset(base) |
//    | MTCz    | Move To Coprocessor                                | x-type   | MTCz     rt, rd           |
//    | MFCz    | Move From Coprocessor                              | x-type   | MFCz     rt, rd           |
//    | CTCz    | Move Control To Coprocessor                        | x-type   | CTCz     rt, rd           |
//    | CFCz    | Move Control From Coprocessor                      | x-type   | CFCz     rt, rd           |
//    | COPz    | Coprocessor Operation                              | x-type   | COPz     cofunc           |
//    | BCzT    | Branch on Coprocessor z True                       | x-type   | BCzT     offset           |
//    | BCzF    | Branch on Coprocessor z False                      | x-type   | BCzF     offset           |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    |         | System Control Coprocessor (CP0) Instructions                                             |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | MTC0    | Move To CP0                                        | x-type   | MTC0 rt, rd               |
//    | MFC0    | Move From CP0                                      | x-type   | MFC0 rt, rd               |
//    | TLBR    | Read indexed TLB entry                             | x-type   | TLBR                      |
//    | TLBWI   | Write indexed TLB entry                            | x-type   | TLBWI                     |
//    | TLBWR   | Write Random TLB entry                             | x-type   | TLBWR                     |
//    | TLBP    | Probe TLB for matching entry                       | x-type   | TLBP                      |
//    | RFE     | Restore From Exception                             | x-type   | RFE                       |
//    +---------+----------------------------------------------------+----------+---------------------------+

const (
	opcodeMask = 0xFC000000 // 0b11111100000000000000000000000000
	sRegMask   = 0x03E00000 // 0b00000011111000000000000000000000
	tRegMask   = 0x001F0000 // 0b00000000000111110000000000000000
	dRegMask   = 0x0000F800 // 0b00000000000000001111100000000000
	aImmMask   = 0x000007C0 // 0b00000000000000000000011111000000
	funcMask   = 0x0000003F // 0b00000000000000000000000000111111
	imm16Mask  = 0x0000FFFF // 0b00000000000000001111111111111111
	imm26Mask  = 0x03FFFFFF // 0b00000011111111111111111111111111
)

// Decode decodes the 4 bytes in src as a single instruction.
func Decode(src []byte) (Inst, error) {
	if len(src) < 4 {
		return Inst{}, errors.New("truncated instruction")
	}
	bits := binary.LittleEndian.Uint32(src)
	opcode := bits & opcodeMask >> 26
	//fmt.Printf("opcode: %06b\n", opcode)
	op := opFromOpcode[opcode]
	if op == invalid {
		// TODO: re-enable panic.
		log.Printf("support for opcode bit pattern %06b not yet implemented", opcode)
		return Inst{}, nil
		//panic(fmt.Errorf("support for opcode bit pattern %06b not yet implemented", opcode))
	}
	switch op {
	case special:
		return decodeRegInst(bits)
	case bcond:
		cond := bits & tRegMask >> 16
		op := opFromCond[cond]
		if op == invalid {
			// TODO: re-enable panic.
			log.Printf("support for cond bit pattern %06b not yet implemented", cond)
			return Inst{}, nil
			//panic(fmt.Errorf("support for cond bit pattern %06b not yet implemented", cond))
		}
		return decodeImmInst(op, bits)
	case J, JAL:
		return decodeJumpInst(op, bits)
	case COP0, COP1, COP2, COP3:
		return decodeCoInst(op, bits)
	default:
		return decodeImmInst(op, bits)
	}
}

// --- [ Instruction with register encoding ] ----------------------------------

// decodeRegInst decodes an instruction with register encoding.
func decodeRegInst(bits uint32) (Inst, error) {
	s := Reg(bits & sRegMask >> 21)
	t := Reg(bits & tRegMask >> 16)
	d := Reg(bits & dRegMask >> 11)
	a := Imm{Imm: bits & aImmMask >> 6, Decimal: true}
	f := bits & funcMask
	//fmt.Printf("s reg:  %05b\n", uint32(s))
	//fmt.Printf("t reg:  %05b\n", uint32(t))
	//fmt.Printf("d reg:  %05b\n", uint32(d))
	//fmt.Printf("a imm:  %05b\n", uint32(a))
	//fmt.Printf("func:   %06b\n", uint32(f))
	op := opFromFunc[f]
	if op == invalid {
		// TODO: re-enable panic.
		log.Printf("support for function bit pattern %06b not yet implemented", f)
		return Inst{}, nil
		//panic(fmt.Errorf("support for function bit pattern %06b not yet implemented", f))
	}
	// +-------------+-----------------+-----------+
	// | Syntax      | Template        | Encoding  |
	// +=============+=================+===========+
	// | ArithLog    | f $d, $s, $t    | Register  |
	// | DivMult     | f $s, $t        | Register  |
	// | Shift       | f $d, $t, a     | Register  |
	// | ShiftV      | f $d, $t, $s    | Register  |
	// | JumpR       | f $s            | Register  |
	// | MoveFrom    | f $d            | Register  |
	// | MoveTo      | f $s            | Register  |
	// +-------------+-----------------+-----------+
	var args Args
	switch op {
	// ArithLog
	case ADD, ADDU, SUB, SUBU, AND, OR, XOR, NOR, SLTU, SLT:
		args[0] = d
		args[1] = s
		args[2] = t
	// DivMult
	case MULT, MULTU, DIV, DIVU:
		args[0] = s
		args[1] = t
	// Shift
	case SLL, SRL, SRA:
		args[0] = d
		args[1] = t
		args[2] = a
	// ShiftV
	case SLLV, SRLV, SRAV:
		args[0] = d
		args[1] = t
		args[2] = s
	// JumpR
	case JR:
		args[0] = s
	case JALR:
		args[0] = s
		args[1] = d
	// MoveFrom
	case MFHI, MFLO:
		args[0] = d
	// MoveTo
	case MTHI, MTLO:
		args[0] = s
	case SYSCALL:
		// TODO: Figure out if syscall takes arguments.
	case BREAK:
		// TODO: Figure out if break takes arguments.
	default:
		// TODO: re-enable panic.
		log.Printf("support for opcode %v not yet implemented", op)
		return Inst{}, nil
		//panic(fmt.Errorf("support for opcode %v not yet implemented", op))
	}
	return Inst{Op: op, Enc: bits, Args: args}, nil
}

// --- [ Instruction with immediate encoding ] ---------------------------------

// decodeImmInst decodes an instruction with immediate encoding.
func decodeImmInst(op Op, bits uint32) (Inst, error) {
	// +-------------+-----------------+-----------+
	// | Syntax      | Template        | Encoding  |
	// +=============+=================+===========+
	// | ArithLogI   | o $t, $s, i     | Immediate |
	// | LoadI       | o $t, immed32   | Immediate |
	// | Branch      | o $s, $t, label | Immediate |
	// | BranchZ     | o $s, label     | Immediate |
	// | LoadStore   | o $t, i($s)     | Immediate |
	// +-------------+-----------------+-----------+
	s := Reg(bits & sRegMask >> 21)
	t := Reg(bits & tRegMask >> 16)
	i := Imm{Imm: bits & imm16Mask}
	//fmt.Printf("s reg:  %05b\n", uint32(s))
	//fmt.Printf("t reg:  %05b\n", uint32(t))
	//fmt.Printf("imm16:  %016b\n", uint32(i))
	var args Args
	switch op {
	// ArithLogI
	case ADDI, ADDIU, SLTIU, SLTI, ANDI, ORI, XORI:
		args[0] = t
		args[1] = s
		args[2] = i
	// LoadI
	case LUI:
		args[0] = t
		args[1] = i
	// Branch
	case BEQ, BNE:
		args[0] = s
		args[1] = t
		args[2] = PCRel(int16(i.Imm) * 4)
	// BranchZ
	case BLEZ, BGTZ, BGEZ, BGEZAL, BLTZ:
		args[0] = s
		args[1] = PCRel(int16(i.Imm) * 4)
	// LoadStore
	case LB, LBU, LH, LHU, LW, LWL, LWR, SB, SH, SW, SWL, SWR:
		args[0] = t
		m := Mem{
			Base:   s,
			Offset: int32(int16(i.Imm)),
		}
		args[1] = m
	default:
		// TODO: re-enable panic.
		log.Printf("support for opcode %v not yet implemented", op)
		return Inst{}, nil
		//panic(fmt.Errorf("support for opcode %v not yet implemented", op))
	}
	return Inst{Op: op, Enc: bits, Args: args}, nil
}

// --- [ Instruction with jump encoding ] --------------------------------------

// decodeJumpInst decodes an instruction with jump encoding.
func decodeJumpInst(op Op, bits uint32) (Inst, error) {
	i := Imm{Imm: bits & imm26Mask}
	//fmt.Printf("imm26:  %026b\n", uint32(i))
	// +-------------+-----------------+-----------+
	// | Syntax      | Template        | Encoding  |
	// +=============+=================+===========+
	// | Jump        | o label         | Jump      |
	// +-------------+-----------------+-----------+
	var args Args
	switch op {
	// Jump
	case J, JAL:
		i.Imm <<= 2
		args[0] = i
	default:
		// TODO: re-enable panic.
		log.Printf("support for opcode %v not yet implemented", op)
		return Inst{}, nil
		//panic(fmt.Errorf("support for opcode %v not yet implemented", op))
	}
	return Inst{Op: op, Enc: bits, Args: args}, nil
}

// --- [ Instruction with co-processor-dependent encoding ] --------------------

// Co-processor computational instructions have co-processor-dependent formats
// (see co-processor manuals).

// decodeCoInst decodes an instruction with co-processor-dependent encoding.
func decodeCoInst(op Op, bits uint32) (Inst, error) {
	//fmt.Printf("data26:  %026b\n", uint32(i))
	// +-------------+-----------------+-----------+
	// | Syntax      | Template        | Encoding  |
	// +=============+=================+===========+
	// +-------------+-----------------+-----------+
	//
	// TODO: Add syntax for co-processor instructions.
	var args Args
	switch op {
	case COP0, COP1, COP2, COP3:
		s := Reg(bits & sRegMask >> 21)
		t := Reg(bits & tRegMask >> 16)
		d := Reg(bits & dRegMask >> 11)
		//a := Imm{Imm: bits & aImmMask >> 6}
		const f5Mask = 0x0000001F // 0b00000000000000000000000000011111
		f5 := bits & f5Mask
		//fmt.Printf("s reg:  %05b\n", uint32(s))
		//fmt.Printf("t reg:  %05b\n", uint32(t))
		//fmt.Printf("d reg:  %05b\n", uint32(d))
		//fmt.Printf("a imm:  %05b\n", uint32(a))
		//fmt.Printf("f5:     %05b\n", uint32(f5))
		if o := opFromCopS[s]; o != invalid {
			// Syntax
			//
			//    MTCz     rt, rd
			//    MFCz     rt, rd
			//    CTCz     rt, rd
			//    CFCz     rt, rd
			switch o {
			case MTC0:
				args[0] = t
				args[1] = C0 + d
			case MFC0:
				args[0] = t
				args[1] = C0 + d
			case CTC0:
				args[0] = t
				args[1] = C0 + d
			case CFC0:
				// TODO: handle arguments.
			case BCC0:
				// TODO: handle arguments.
			default:
				// TODO: re-enable panic.
				log.Printf("support for co-processor opcode %v not yet implemented", o)
				return Inst{}, nil
				//panic(fmt.Errorf("support for co-processor opcode %v not yet implemented", o))
			}
			op = fixCoID(op, o)
		} else if o := opFromCopT[t]; o != invalid {
			// TODO: Implement.
			log.Printf("support for co-processor opcode t %v not yet implemented", o)
			op = fixCoID(op, o)
			return Inst{}, nil
		} else if o := opFromCop0[f5]; o != invalid {
			// TODO: Implement.
			log.Printf("support for co-processor opcode f5 %v not yet implemented", o)
			op = fixCoID(op, o)
		} else {
			// TODO: Validate argument.
			const imm24Mask = 0x00FFFFFF // 0b00000000111111111111111111111111
			i := Imm{Imm: bits & imm24Mask}
			args[0] = i
		}

		//args[0] = i // TODO: Figure out which COP instructions that should have
		//arguments.
	default:
		// TODO: re-enable panic.
		log.Printf("support for opcode %v not yet implemented", op)
		return Inst{}, nil
		//panic(fmt.Errorf("support for opcode %v not yet implemented", op))
	}
	return Inst{Op: op, Enc: bits, Args: args}, nil
}

// ### [ Helper functions ] ####################################################

// fixCoID fixes the co-processor ID of the opcode.
func fixCoID(orig, op Op) Op {
	n := 0
	switch orig {
	case COP0:
		n = 0
	case COP1:
		n = 1
	case COP2:
		n = 2
	case COP3:
		n = 3
	default:
		panic(fmt.Errorf("invalid COP opcode; expected COP0-3, got %v", orig))
	}
	switch op {
	case MFC0, MTC0, CFC0, CTC0, BCC0, BC0F, BC0T:
		return op + Op(n)
	case TLBR, TLBWI, TLBWR, TLBP, RFE:
		return op // always on CO0
	}
	return orig
}

// Reference. The IDTR3051, R3052 RISController Hardware User's Manual.
//
// Chapter 2: INSTRUCTION SET ARCHITECTURE. R3051 OPCODE ENCODING

// opFromOpcode maps from opcode bit pattern to MIPS opcode.
var opFromOpcode = [...]Op{
	0x00: special, // 0b000000
	0x01: bcond,   // 0b000001
	0x02: J,       // 0b000010
	0x03: JAL,     // 0b000011
	0x04: BEQ,     // 0b000100
	0x05: BNE,     // 0b000101
	0x06: BLEZ,    // 0b000110
	0x07: BGTZ,    // 0b000111
	0x08: ADDI,    // 0b001000
	0x09: ADDIU,   // 0b001001
	0x0A: SLTI,    // 0b001010
	0x0B: SLTIU,   // 0b001011
	0x0C: ANDI,    // 0b001100
	0x0D: ORI,     // 0b001101
	0x0E: XORI,    // 0b001110
	0x0F: LUI,     // 0b001111
	0x10: COP0,    // 0b010000
	0x11: COP1,    // 0b010001
	0x12: COP2,    // 0b010010
	0x13: COP3,    // 0b010011
	0x14: invalid, // 0b010100
	0x15: invalid, // 0b010101
	0x16: invalid, // 0b010110
	0x17: invalid, // 0b010111
	0x18: invalid, // 0b011000
	0x19: invalid, // 0b011001
	0x1A: invalid, // 0b011010
	0x1B: invalid, // 0b011011
	0x1C: invalid, // 0b011100
	0x1D: invalid, // 0b011101
	0x1E: invalid, // 0b011110
	0x1F: invalid, // 0b011111
	0x20: LB,      // 0b100000
	0x21: LH,      // 0b100001
	0x22: LWL,     // 0b100010
	0x23: LW,      // 0b100011
	0x24: LBU,     // 0b100100
	0x25: LHU,     // 0b100101
	0x26: LWR,     // 0b100110
	0x27: invalid, // 0b100111
	0x28: SB,      // 0b101000
	0x29: SH,      // 0b101001
	0x2A: SWL,     // 0b101010
	0x2B: SW,      // 0b101011
	0x2C: invalid, // 0b101100
	0x2D: invalid, // 0b101101
	0x2E: SWR,     // 0b101110
	0x2F: invalid, // 0b101111
	0x30: LWC0,    // 0b110000
	0x31: LWC1,    // 0b110001
	0x32: LWC2,    // 0b110010
	0x33: LWC3,    // 0b110011
	0x34: invalid, // 0b110100
	0x35: invalid, // 0b110101
	0x36: invalid, // 0b110110
	0x37: invalid, // 0b110111
	0x38: SWC0,    // 0b111000
	0x39: SWC1,    // 0b111001
	0x3A: SWC2,    // 0b111010
	0x3B: SWC3,    // 0b111011
	0x3C: invalid, // 0b111100
	0x3D: invalid, // 0b111101
	0x3E: invalid, // 0b111110
	0x3F: invalid, // 0b111111
}

// opFromFunc maps from function bit pattern to MIPS opcode.
var opFromFunc = [...]Op{
	0x00: SLL,     // 0b000000
	0x01: invalid, // 0b000001
	0x02: SRL,     // 0b000010
	0x03: SRA,     // 0b000011
	0x04: SLLV,    // 0b000100
	0x05: invalid, // 0b000101
	0x06: SRLV,    // 0b000110
	0x07: SRAV,    // 0b000111
	0x08: JR,      // 0b001000
	0x09: JALR,    // 0b001001
	0x0A: invalid, // 0b001010
	0x0B: invalid, // 0b001011
	0x0C: SYSCALL, // 0b001100
	0x0D: BREAK,   // 0b001101
	0x0E: invalid, // 0b001110
	0x0F: invalid, // 0b001111
	0x10: MFHI,    // 0b010000
	0x11: MTHI,    // 0b010001
	0x12: MFLO,    // 0b010010
	0x13: MTLO,    // 0b010011
	0x14: invalid, // 0b010100
	0x15: invalid, // 0b010101
	0x16: invalid, // 0b010110
	0x17: invalid, // 0b010111
	0x18: MULT,    // 0b011000
	0x19: MULTU,   // 0b011001
	0x1A: DIV,     // 0b011010
	0x1B: DIVU,    // 0b011011
	0x1C: invalid, // 0b011100
	0x1D: invalid, // 0b011101
	0x1E: invalid, // 0b011110
	0x1F: invalid, // 0b011111
	0x20: ADD,     // 0b100000
	0x21: ADDU,    // 0b100001
	0x22: SUB,     // 0b100010
	0x23: SUBU,    // 0b100011
	0x24: AND,     // 0b100100
	0x25: OR,      // 0b100101
	0x26: XOR,     // 0b100110
	0x27: NOR,     // 0b100111
	0x28: invalid, // 0b101000
	0x29: invalid, // 0b101001
	0x2A: SLT,     // 0b101010
	0x2B: SLTU,    // 0b101011
	0x2C: invalid, // 0b101100
	0x2D: invalid, // 0b101101
	0x2E: invalid, // 0b101110
	0x2F: invalid, // 0b101111
	0x30: invalid, // 0b110000
	0x31: invalid, // 0b110001
	0x32: invalid, // 0b110010
	0x33: invalid, // 0b110011
	0x34: invalid, // 0b110100
	0x35: invalid, // 0b110101
	0x36: invalid, // 0b110110
	0x37: invalid, // 0b110111
	0x38: invalid, // 0b111000
	0x39: invalid, // 0b111001
	0x3A: invalid, // 0b111010
	0x3B: invalid, // 0b111011
	0x3C: invalid, // 0b111100
	0x3D: invalid, // 0b111101
	0x3E: invalid, // 0b111110
	0x3F: invalid, // 0b111111
}

// opFromCond maps from branch condition bit pattern to MIPS opcode.
var opFromCond = [...]Op{
	0x00: BLTZ,    // 0b00000
	0x01: BGEZ,    // 0b00001
	0x02: invalid, // 0b00010
	0x03: invalid, // 0b00011
	0x04: invalid, // 0b00100
	0x05: invalid, // 0b00101
	0x06: invalid, // 0b00110
	0x07: invalid, // 0b00111
	0x08: invalid, // 0b01000
	0x09: invalid, // 0b01001
	0x0A: invalid, // 0b01010
	0x0B: invalid, // 0b01011
	0x0C: invalid, // 0b01100
	0x0D: invalid, // 0b01101
	0x0E: invalid, // 0b01110
	0x0F: invalid, // 0b01111
	0x10: BLTZAL,  // 0b10000
	0x11: BGEZAL,  // 0b10001
	0x12: invalid, // 0b10010
	0x13: invalid, // 0b10011
	0x14: invalid, // 0b10100
	0x15: invalid, // 0b10101
	0x16: invalid, // 0b10110
	0x17: invalid, // 0b10111
	0x18: invalid, // 0b11000
	0x19: invalid, // 0b11001
	0x1A: invalid, // 0b11010
	0x1B: invalid, // 0b11011
	0x1C: invalid, // 0b11100
	0x1D: invalid, // 0b11101
	0x1E: invalid, // 0b11110
	0x1F: invalid, // 0b11111
}

// TODO: Add tables for COPz, BCzF/T and CP0 from page 37.

// opFromCopS maps from co-processor bit pattern (bits[21:26]) to MIPS opcode.
var opFromCopS = [...]Op{
	0x00: MFC0,    // 0b00000; actual co-processor ID determined by COPz
	0x01: invalid, // 0b00001
	0x02: CFC0,    // 0b00010; actual co-processor ID determined by COPz
	0x03: invalid, // 0b00011
	0x04: MTC0,    // 0b00100; actual co-processor ID determined by COPz
	0x05: invalid, // 0b00101
	0x06: CTC0,    // 0b00110; actual co-processor ID determined by COPz
	0x07: invalid, // 0b00111
	0x08: BCC0,    // 0b01000; actual co-processor ID determined by COPz
	0x09: invalid, // 0b01001
	0x0A: invalid, // 0b01010
	0x0B: invalid, // 0b01011
	0x0C: invalid, // 0b01100
	0x0D: invalid, // 0b01101
	0x0E: invalid, // 0b01110
	0x0F: invalid, // 0b01111
	// Co-Processor Specific Operations.
	0x10: invalid, // 0b10000
	0x11: invalid, // 0b10001
	0x12: invalid, // 0b10010
	0x13: invalid, // 0b10011
	0x14: invalid, // 0b10100
	0x15: invalid, // 0b10101
	0x16: invalid, // 0b10110
	0x17: invalid, // 0b10111
	0x18: invalid, // 0b11000
	0x19: invalid, // 0b11001
	0x1A: invalid, // 0b11010
	0x1B: invalid, // 0b11011
	0x1C: invalid, // 0b11100
	0x1D: invalid, // 0b11101
	0x1E: invalid, // 0b11110
	0x1F: invalid, // 0b11111
}

// opFromCopT maps from co-processor bit pattern (bits[16:21]) to MIPS opcode.
var opFromCopT = [...]Op{
	0x00: BC0F,    // 0b00000; actual co-processor ID determined by COPz
	0x01: BC0T,    // 0b00001; actual co-processor ID determined by COPz
	0x02: invalid, // 0b00010
	0x03: invalid, // 0b00011
	0x04: invalid, // 0b00100
	0x05: invalid, // 0b00101
	0x06: invalid, // 0b00110
	0x07: invalid, // 0b00111
	0x08: invalid, // 0b01000
	0x09: invalid, // 0b01001
	0x0A: invalid, // 0b01010
	0x0B: invalid, // 0b01011
	0x0C: invalid, // 0b01100
	0x0D: invalid, // 0b01101
	0x0E: invalid, // 0b01110
	0x0F: invalid, // 0b01111
	0x10: invalid, // 0b10000
	0x11: invalid, // 0b10001
	0x12: invalid, // 0b10010
	0x13: invalid, // 0b10011
	0x14: invalid, // 0b10100
	0x15: invalid, // 0b10101
	0x16: invalid, // 0b10110
	0x17: invalid, // 0b10111
	0x18: invalid, // 0b11000
	0x19: invalid, // 0b11001
	0x1A: invalid, // 0b11010
	0x1B: invalid, // 0b11011
	0x1C: invalid, // 0b11100
	0x1D: invalid, // 0b11101
	0x1E: invalid, // 0b11110
	0x1F: invalid, // 0b11111
}

// opFromCop0 maps from co-processor bit pattern (bits[0:4]) to MIPS opcode.
var opFromCop0 = [...]Op{
	0x00: invalid, // 0b00000
	0x01: TLBR,    // 0b00001
	0x02: TLBWI,   // 0b00010
	0x03: invalid, // 0b00011
	0x04: invalid, // 0b00100
	0x05: invalid, // 0b00101
	0x06: TLBWR,   // 0b00110
	0x07: invalid, // 0b00111
	0x08: TLBP,    // 0b01000
	0x09: invalid, // 0b01001
	0x0A: invalid, // 0b01010
	0x0B: invalid, // 0b01011
	0x0C: invalid, // 0b01100
	0x0D: invalid, // 0b01101
	0x0E: invalid, // 0b01110
	0x0F: invalid, // 0b01111
	0x10: RFE,     // 0b10000
	0x11: invalid, // 0b10001
	0x12: invalid, // 0b10010
	0x13: invalid, // 0b10011
	0x14: invalid, // 0b10100
	0x15: invalid, // 0b10101
	0x16: invalid, // 0b10110
	0x17: invalid, // 0b10111
	0x18: invalid, // 0b11000
	0x19: invalid, // 0b11001
	0x1A: invalid, // 0b11010
	0x1B: invalid, // 0b11011
	0x1C: invalid, // 0b11100
	0x1D: invalid, // 0b11101
	0x1E: invalid, // 0b11110
	0x1F: invalid, // 0b11111
}

// The destination address of jump instructions is (pc&0xF0000000 + target);
// where pc is the address of the jump instruction -- from which the 4 most
// significant bits are used -- and target is the immediate shifted two bits to
// the left.

// The destination address of a branch instruction is pc + target; where pc is
// the address of the delay slot instruction and target is the immediate shifted
// two bits to the left.
