package mips

import (
	"encoding/binary"
	"fmt"
	"log"

	"github.com/pkg/errors"
)

// References.
//
// [1]: The IDTR3051, R3052 RISController Hardware User's Manual.
// [2]: MIPS32 Architecture For Programmers Volume II: The MIPS32 Instruction Set
// [3]: IDT R30xx Family Software Reference Manual

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
//    | LB      | Load Byte                                          | I-type   | LB      $t, offset($s)    |
//    | LBU     | Load Byte Unsigned                                 | I-type   | LBU     $t, offset($s)    |
//    | LH      | Load Halfword                                      | I-type   | LH      $t, offset($s)    |
//    | LHU     | Load Halfword Unsigned                             | I-type   | LHU     $t, offset($s)    |
//    | LW      | Load Word                                          | I-type   | LW      $t, offset($s)    |
//    | LWL     | Load Word Left                                     | I-type   | LWL     $t, offset($s)    |
//    | LWR     | Load Word Right                                    | I-type   | LWR     $t, offset($s)    |
//    | SB      | Store Byte                                         | I-type   | SB      $t, offset($s)    |
//    | SH      | Store Halfword                                     | I-type   | SH      $t, offset($s)    |
//    | SW      | Store Word                                         | I-type   | SW      $t, offset($s)    |
//    | SWL     | Store Word Left                                    | I-type   | SWL     $t, offset($s)    |
//    | SWR     | Store Word Right                                   | I-type   | SWR     $t, offset($s)    |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    |         | Arithmetic Instructions (ALU Immediate)                                                   |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | ADDI    | Add Immediate                                      | I-type   | ADDI    $t, $s, immediate |
//    | ADDIU   | Add Immediate Unsigned                             | I-type   | ADDIU   $t, $s, immediate |
//    | SLTI    | Set on Less Than Immediate                         | I-type   | SLTI    $t, $s, immediate |
//    | SLTIU   | Set on Less Than Immediate Unsigned                | I-type   | SLTIU   $t, $s, immediate |
//    | ANDI    | AND Immediate                                      | I-type   | ANDI    $t, $s, immediate |
//    | ORI     | OR Immediate                                       | I-type   | ORI     $t, $s, immediate |
//    | XORI    | Exclusive OR Immediate                             | I-type   | XORI    $t, $s, immediate |
//    | LUI     | Load Upper Immediate                               | I-type   | LUI     $t, immediate     |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    |         | Arithmetic Instructions (3-operand, register-type)                                        |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | ADD     | Add                                                | R-type   | ADD     $d, $s, $t        |
//    | ADDU    | Add Unsigned                                       | R-type   | ADDU    $d, $s, $t        |
//    | SUB     | Subtract                                           | R-type   | SUB     $d, $s, $t        |
//    | SUBU    | Subtract Unsigned                                  | R-type   | SUBU    $d, $s, $t        |
//    | SLT     | Set on Less Than                                   | R-type   | SLT     $d, $s, $t        |
//    | SLTU    | Set on Less Than Unsigned                          | R-type   | SLTU    $d, $s, $t        |
//    | AND     | AND                                                | R-type   | AND     $d, $s, $t        |
//    | OR      | OR                                                 | R-type   | OR      $d, $s, $t        |
//    | XOR     | Exclusive OR                                       | R-type   | XOR     $d, $s, $t        |
//    | NOR     | NOR                                                | R-type   | NOR     $d, $s, $t        |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    |         | Shift Instructions                                                                        |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | SLL     | Shift Left Logical                                 | R-type   | SLL     $d, $t, shift     |
//    | SRL     | Shift Right Logical                                | R-type   | SRL     $d, $t, shift     |
//    | SRA     | Shift Right Arithmetic                             | R-type   | SRA     $d, $t, shift     |
//    | SLLV    | Shift Left Logical Variable                        | R-type   | SLLV    $d, $t, $s        |
//    | SRLV    | Shift Right Logical Variable                       | R-type   | SRLV    $d, $t, $s        |
//    | SRAV    | Shift Right Arithmetic Variable                    | R-type   | SRAV    $d, $t, $s        |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    |         | Multiply/Divide Instructions                                                              |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | MULT    | Multiply                                           | R-type   | MULT    $s, $t            |
//    | MULTU   | Multiply Unsigned                                  | R-type   | MULTU   $s, $t            |
//    | DIV     | Divide                                             | R-type   | DIV     $s, $t            |
//    | DIVU    | Divide Unsigned                                    | R-type   | DIVU    $s, $t            |
//    | MFHI    | Move From HI                                       | R-type   | MFHI    $d                |
//    | MTHI    | Move To HI                                         | R-type   | MTHI    $s                |
//    | MFLO    | Move From LO                                       | R-type   | MFLO    $d                |
//    | MTLO    | Move To LO                                         | R-type   | MTLO    $s                |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    |         | Jump and Branch Instructions                                                              |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | J       | Jump                                               | J-type   | J       target            |
//    | JAL     | Jump and Link                                      | J-type   | JAL     target            |
//    | JR      | Jump to Register                                   | R-type   | JR      $s                |
//    | JALR    | Jump and Link Register                             | R-type   | JALR    $s, $d            |
//    | BEQ     | Branch on Equal                                    | I-type   | BEQ     $s, $t, offset    |
//    | BNE     | Branch on Not Equal                                | I-type   | BNE     $s, $t, offset    |
//    | BLEZ    | Branch on Less than or Equal to Zero               | I-type   | BLEZ    $s, offset        |
//    | BGTZ    | Branch on Greater Than Zero                        | I-type   | BGTZ    $s, offset        |
//    | BLTZ    | Branch on Less Than Zero                           | I-type   | BLTZ    $s, offset        |
//    | BGEZ    | Branch on Greater Than or Equal to Zero            | I-type   | BGEZ    $s, offset        |
//    | BLTZAL  | Branch on Less Than Zero and Link                  | I-type   | BLTZAL  $s, offset        |
//    | BGEZAL  | Branch on Greater Than or Equal to Zero and Link   | I-type   | BGEZAL  $s, offset        |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    |         | Special Instructions                                                                      |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | SYSCALL | System Call                                        | R-type   | SYSCALL code              |
//    | BREAK   | Break                                              | R-type   | BREAK   code              |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    |         | Coprocessor Instructions                                                                  |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | LWCz    | Load Word from Coprocessor                         | I-type   | LWCz     $t, offset($s)   |
//    | SWCz    | Store Word to Coprocessor                          | I-type   | SWCz     $t, offset($s)   |
//    | MTCz    | Move To Coprocessor                                | x-type   | MTCz     $t, $d           |
//    | MFCz    | Move From Coprocessor                              | x-type   | MFCz     $t, $d           |
//    | CTCz    | Move Control To Coprocessor                        | x-type   | CTCz     $t, $d           |
//    | CFCz    | Move Control From Coprocessor                      | x-type   | CFCz     $t, $d           |
//    | COPz    | Coprocessor Operation                              | x-type   | COPz     cofunc           |
//    | BCzT    | Branch on Coprocessor z True                       | x-type   | BCzT     offset           |
//    | BCzF    | Branch on Coprocessor z False                      | x-type   | BCzF     offset           |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    |         | System Control Coprocessor (CP0) Instructions                                             |
//    +---------+----------------------------------------------------+----------+---------------------------+
//    | MTC0    | Move To CP0                                        | x-type   | MTC0     $t, $d           |
//    | MFC0    | Move From CP0                                      | x-type   | MFC0     $t, $d           |
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
	op := opFromOpcode[opcode]
	switch op {
	case invalid:
		return Inst{}, errors.Errorf("support for opcode bit pattern %06b not yet implemented", opcode)
	case special:
		return decodeRegInst(bits)
	case bcond:
		cond := bits & tRegMask >> 16
		op := opFromCond[cond]
		if op == invalid {
			return Inst{}, errors.Errorf("support for conditional bit pattern %06b not yet implemented", cond)
		}
		return decodeImmInst(op, bits)
	case J, JAL:
		return decodeJumpInst(op, bits)
	// Coprocessor Instructions
	case COP0, COP1, COP2, COP3:
		return decodeCoInst(op, bits)
	default:
		return decodeImmInst(op, bits)
	}
}

// --- [ R-type ] --------------------------------------------------------------

// decodeRegInst decodes an instruction with register encoding.
func decodeRegInst(bits uint32) (Inst, error) {
	var args Args
	s := Reg(bits & sRegMask >> 21)
	t := Reg(bits & tRegMask >> 16)
	d := Reg(bits & dRegMask >> 11)
	a := Imm{Imm: bits & aImmMask >> 6, Decimal: true}
	f := bits & funcMask
	op := opFromFunc[f]
	switch op {
	case invalid:
		return Inst{}, errors.Errorf("support for function bit pattern %06b not yet implemented", f)

	// Arithmetic Instructions (3-operand, register-type)
	case ADD, ADDU, SUB, SUBU, SLT, SLTU, AND, OR, XOR, NOR:
		// Syntax.
		//
		//    ADD     $d, $s, $t
		//    ADDU    $d, $s, $t
		//    SUB     $d, $s, $t
		//    SUBU    $d, $s, $t
		//    SLT     $d, $s, $t
		//    SLTU    $d, $s, $t
		//    AND     $d, $s, $t
		//    OR      $d, $s, $t
		//    XOR     $d, $s, $t
		//    NOR     $d, $s, $t
		args[0] = d
		args[1] = s
		args[2] = t

	// Shift Instructions
	case SLL, SRL, SRA:
		// Syntax.
		//
		//    SLL     $d, $t, shift
		//    SRL     $d, $t, shift
		//    SRA     $d, $t, shift
		args[0] = d
		args[1] = t
		args[2] = a
	case SLLV, SRLV, SRAV:
		// Syntax.
		//
		//    SLLV    $d, $t, $s
		//    SRLV    $d, $t, $s
		//    SRAV    $d, $t, $s
		args[0] = d
		args[1] = t
		args[2] = s

	// Multiply/Divide Instructions
	case MULT, MULTU, DIV, DIVU:
		// Syntax.
		//
		//    MULT    $s, $t
		//    MULTU   $s, $t
		//    DIV     $s, $t
		//    DIVU    $s, $t
		args[0] = s
		args[1] = t
	case MFHI, MFLO:
		// Syntax.
		//
		//    MFHI    $d
		//    MFLO    $d
		args[0] = d
	case MTHI, MTLO:
		// Note, the syntax of MTHI and MTLO was incorrect in [1]. The correct
		// syntax is specified in [2].

		// Syntax.
		//
		//    MTHI    $s
		//    MTLO    $s
		args[0] = s

	// Jump and Branch Instructions
	case JR:
		// Syntax.
		//
		//    JR      $s
		args[0] = s
	case JALR:
		// Syntax.
		//
		//    JALR    $s, $d
		args[0] = s
		args[1] = d

	// Special Instructions
	case SYSCALL, BREAK:
		// Syntax.
		//
		//    SYSCALL code
		//    BREAK   code
		const codeMask = 0x03FFFFC0 // 0b00000011111111111111111111000000
		code := Imm{Imm: bits & codeMask >> 6}
		args[0] = code

	default:
		panic(fmt.Errorf("support for opcode %v not yet implemented", op))
	}
	return Inst{Op: op, Enc: bits, Args: args}, nil
}

// --- [ I-type ] --------------------------------------------------------------

// The destination address of a branch instruction is pc + target; where pc is
// the address of the delay slot instruction and target is the immediate shifted
// two bits to the left.

// decodeImmInst decodes an instruction with immediate encoding.
func decodeImmInst(op Op, bits uint32) (Inst, error) {
	var args Args
	s := Reg(bits & sRegMask >> 21)
	t := Reg(bits & tRegMask >> 16)
	i := Imm{Imm: uint32(int16(bits & imm16Mask))}
	switch op {
	// Load/Store Instructions
	case LB, LBU, LH, LHU, LW, LWL, LWR, SB, SH, SW, SWL, SWR:
		// Syntax.
		//
		//    LB      $t, offset($s)
		//    LBU     $t, offset($s)
		//    LH      $t, offset($s)
		//    LHU     $t, offset($s)
		//    LW      $t, offset($s)
		//    LWL     $t, offset($s)
		//    LWR     $t, offset($s)
		//    SB      $t, offset($s)
		//    SH      $t, offset($s)
		//    SW      $t, offset($s)
		//    SWL     $t, offset($s)
		//    SWR     $t, offset($s)
		args[0] = t
		m := Mem{
			Base:   s,
			Offset: int32(i.Imm),
		}
		args[1] = m

	// Arithmetic Instructions (ALU Immediate)
	case ADDI, ADDIU, SLTI, SLTIU, ANDI, ORI, XORI:
		// Syntax.
		//
		//    ADDI    $t, $s, immediate
		//    ADDIU   $t, $s, immediate
		//    SLTI    $t, $s, immediate
		//    SLTIU   $t, $s, immediate
		//    ANDI    $t, $s, immediate
		//    ORI     $t, $s, immediate
		//    XORI    $t, $s, immediate
		args[0] = t
		args[1] = s
		args[2] = i
	case LUI:
		// Syntax.
		//
		//    LUI     $t, immediate
		args[0] = t
		args[1] = i

	// Jump and Branch Instructions

	case BEQ, BNE:
		// Syntax.
		//
		//    BEQ     $s, $t, offset
		//    BNE     $s, $t, offset
		args[0] = s
		args[1] = t
		args[2] = PCRel(int16(i.Imm) * 4)
	case BLEZ, BGTZ, BLTZ, BGEZ, BLTZAL, BGEZAL:
		// Syntax.
		//
		//    BLEZ    $s, offset
		//    BGTZ    $s, offset
		//    BLTZ    $s, offset
		//    BGEZ    $s, offset
		//    BLTZAL  $s, offset
		//    BGEZAL  $s, offset
		args[0] = s
		args[1] = PCRel(int16(i.Imm) * 4)

	// Coprocessor Instructions
	case LWC0, LWC1, LWC2, LWC3, SWC0, SWC1, SWC2, SWC3:
		// Syntax.
		//
		//    LWCz     $t, offset($s)
		//    SWCz     $t, offset($s)
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

// --- [ J-type ] --------------------------------------------------------------

// The destination address of jump instructions is (pc&0xF0000000 + target);
// where pc is the address of the jump instruction -- from which the 4 most
// significant bits are used -- and target is the immediate shifted two bits to
// the left.

// decodeJumpInst decodes an instruction with jump encoding.
func decodeJumpInst(op Op, bits uint32) (Inst, error) {
	var args Args
	i := Imm{Imm: bits & imm26Mask}
	switch op {
	// Jump and Branch Instructions
	case J, JAL:
		// Syntax.
		//
		//    J       target
		//    JAL     target
		i.Imm <<= 2
		args[0] = i
	default:
		panic(fmt.Errorf("support for opcode %v not yet implemented", op))
	}
	return Inst{Op: op, Enc: bits, Args: args}, nil
}

// --- [ x-type ] --------------------------------------------------------------

// Co-processor computational instructions have co-processor-dependent formats
// (see co-processor manuals).

// decodeCoInst decodes an instruction with co-processor-dependent encoding.
func decodeCoInst(op Op, bits uint32) (Inst, error) {
	var args Args
	switch op {
	case COP0, COP1, COP2, COP3:
		const subopMask = 0x03E00000 // 0b00000011111000000000000000000000
		subop := bits & subopMask >> 21
		// Co-Processor Specific Operations.
		const coMask = 0x10 // 0b10000
		if subop&coMask != 0 {
			switch op {
			case COP0:
				const cop0Mask = 0x0000001F // 0b00000000000000000000000000011111
				cop0 := bits & cop0Mask
				o := opFromCop0[cop0]
				switch o {
				case invalid:
					return Inst{}, errors.Errorf("support for COP0 bit pattern %05b not yet implemented", cop0)
				// System Control Coprocessor (CP0) Instructions
				case TLBR, TLBWI, TLBWR, TLBP, RFE:
					// Syntax.
					//
					//    TLBR
					//    TLBWI
					//    TLBWR
					//    TLBP
					//    RFE
					return Inst{Op: o, Enc: bits, Args: args}, nil
				default:
					panic(fmt.Errorf("support for COP0 opcode %v not yet implemented", o))
				}
			case COP1:
				// TODO: implement COP1 specific instructions.
				// TODO: re-enable panic.
				log.Print("support for COP1 specific operations not yet implemented")
				return Inst{}, nil
				//panic("support for COP1 specific operations not yet implemented")
			case COP2:
				const cofuncMask = 0x01FFFFFF // 0b00000001111111111111111111111111
				cofunc := Imm{Imm: bits & cofuncMask}
				args[0] = cofunc
				return Inst{Op: op, Enc: bits, Args: args}, nil
			case COP3:
				// TODO: implement COP3 specific instructions.
				// TODO: re-enable panic.
				log.Print("support for COP3 specific operations not yet implemented")
				return Inst{}, nil
				//panic("support for COP3 specific operations not yet implemented")
			}
		}
		z := counit(op)
		o := opFromCoSubop[subop]
		switch o {
		case invalid:
			// TODO: re-enable panic.
			log.Printf("support for co-processor suboperation bit pattern %06b not yet implemented", subop)
			return Inst{}, nil
			//panic(fmt.Sprintf("support for co-processor suboperation bit pattern %06b not yet implemented", subop))
		// System Control Coprocessor (CP0) Instructions
		case MTC0, MFC0, CTC0, CFC0:
			t := Reg(bits & tRegMask >> 16)
			d := Reg(bits & dRegMask >> 11)
			// Syntax
			//
			//    MTCz     $t, $d
			//    MFCz     $t, $d
			//    CTCz     $t, $d
			//    CFCz     $t, $d
			args[0] = t
			args[1] = coreg(d, z)
			return Inst{Op: coop(o, z), Enc: bits, Args: args}, nil
		// Coprocessor Instructions
		case bc:
			const bccondMask = 0x03E00000 // 0b00000011111000000000000000000000
			bccond := bits & bccondMask >> 21
			o := opFromBCCond[bccond]
			i := Imm{Imm: bits & imm16Mask}
			args[0] = i
			return Inst{Op: coop(o, z), Enc: bits, Args: args}, nil
		default:
			panic(fmt.Errorf("support for opcode %v not yet implemented", o))
		}
	default:
		// TODO: re-enable panic.
		log.Printf("support for opcode %v not yet implemented", op)
		return Inst{}, nil
		//panic(fmt.Errorf("support for opcode %v not yet implemented", op))
	}
}

// ### [ Helper functions ] ####################################################

// counit returns the co-processor unit ID from the given COPz opcode.
func counit(op Op) int {
	switch op {
	case COP0:
		return 0
	case COP1:
		return 1
	case COP2:
		return 2
	case COP3:
		return 3
	default:
		panic(fmt.Errorf("invalid COPz opcode; expected COP0, COP1, COP2 or COP3, got %v", op))
	}
}

// coreg returns the registers r of co-processor unit z.
func coreg(r Reg, z int) Reg {
	// Number of registers in co-processor.
	const ncoregs = 32
	first := CP0Reg0 + Reg(z*ncoregs)
	return first + r
}

// coop returns the operation op of co-processor unit z.
func coop(op Op, z int) Op {
	switch op {
	case MFC0, MTC0, CFC0, CTC0, BC0F, BC0T:
		return op + Op(z)
	case TLBR, TLBWI, TLBWR, TLBP, RFE:
		return op // always on CO0
	default:
		panic(fmt.Errorf("support for opcode %v not yet implemented", op))
	}
}

// ref: Chapter 2: INSTRUCTION SET ARCHITECTURE. R3051 OPCODE ENCODING [1]

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

// opFromCoSubop maps from co-processor suboperation bit pattern (bits[21:26])
// to MIPS opcode.
var opFromCoSubop = [...]Op{
	0x00: MFC0,    // 0b00000; actual co-processor ID determined by COPz
	0x01: invalid, // 0b00001
	0x02: CFC0,    // 0b00010; actual co-processor ID determined by COPz
	0x03: invalid, // 0b00011
	0x04: MTC0,    // 0b00100; actual co-processor ID determined by COPz
	0x05: invalid, // 0b00101
	0x06: CTC0,    // 0b00110; actual co-processor ID determined by COPz
	0x07: invalid, // 0b00111
	0x08: bc,      // 0b01000; actual co-processor ID determined by COPz
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

// opFromBCCond maps from COPz BC condition bit pattern (bits[16:21]) to MIPS
// opcode.
var opFromBCCond = [...]Op{
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

// opFromCop0 maps from COP0 bit pattern (bits[0:4]) to MIPS opcode.
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
