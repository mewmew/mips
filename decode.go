package mips

import (
	"encoding/binary"
	"fmt"

	"github.com/pkg/errors"
)

// Instruction encodings
//
//    Register    000000ss sssttttt dddddaaa aaffffff
//    Immediate   ooooooss sssttttt iiiiiiii iiiiiiii
//    Jump        ooooooii iiiiiiii iiiiiiii iiiiiiii

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
	fmt.Printf("opcode: %06b\n", opcode)
	if opcode == 0 {
		return decodeRegInst(bits)
	}
	op, ok := opFromOpcode[opcode]
	if !ok {
		panic(fmt.Errorf("support for opcode bit pattern %06b not yet implemented", opcode))
	}
	switch op {
	case J, JAL, TRAP:
		return decodeJumpInst(op, bits)
	default:
		return decodeImmInst(op, bits)
	}
	return Inst{}, nil
}

// --- [ Instruction with register encoding ] ----------------------------------

// Instructions with register encoding:
//
//    syntax     func     op
//
//    ArithLog   100000   add
//    ArithLog   100001   addu
//    ArithLog   100010   sub
//    ArithLog   100011   subu
//    ArithLog   100100   and
//    ArithLog   100101   or
//    ArithLog   100110   xor
//    ArithLog   100111   nor
//    ArithLog   101001   sltu
//    ArithLog   101010   slt
//    DivMult    011000   mult
//    DivMult    011001   multu
//    DivMult    011010   div
//    DivMult    011011   divu
//    JumpR      001000   jr
//    JumpR      001001   jalr
//    MoveFrom   010000   mfhi
//    MoveFrom   010010   mflo
//    MoveTo     010001   mthi
//    MoveTo     010011   mtlo
//    Shift      000000   sll
//    Shift      000010   srl
//    Shift      000011   sra
//    ShiftV     000100   sllv
//    ShiftV     000110   srlv
//    ShiftV     000111   srav

// decodeRegInst decodes an instruction with register encoding.
func decodeRegInst(bits uint32) (Inst, error) {
	s := Reg(bits & sRegMask >> 21)
	t := Reg(bits & tRegMask >> 16)
	d := Reg(bits & dRegMask >> 11)
	a := Imm(bits & aImmMask >> 6)
	f := bits & funcMask
	fmt.Printf("s reg:  %05b\n", uint32(s))
	fmt.Printf("t reg:  %05b\n", uint32(t))
	fmt.Printf("d reg:  %05b\n", uint32(d))
	fmt.Printf("a imm:  %05b\n", uint32(a))
	fmt.Printf("func:   %06b\n", uint32(f))
	op, ok := opFromFunc[f]
	if !ok {
		panic(fmt.Errorf("support for function bit pattern %06b not yet implemented", f))
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
		args[0] = t
		args[0] = a
	// ShiftV
	case SLLV, SRLV, SRAV:
		args[0] = d
		args[1] = t
		args[2] = s
	// JumpR
	case JR, JALR:
		args[0] = s
	// MoveFrom
	case MFHI, MFLO:
		args[0] = d
	// MoveTo
	case MTHI, MTLO:
		args[0] = s
	default:
		panic(fmt.Errorf("support for opcode %v not yet implemented", op))
	}
	return Inst{Op: op, Enc: bits, Args: args}, nil
}

// --- [ Instruction with immediate encoding ] ---------------------------------

// Instructions with immediate encoding:
//
//    syntax      opcode   op
//
//    ArithLogI   001000   addi
//    ArithLogI   001001   addiu
//    ArithLogI   001001   sltiu
//    ArithLogI   001010   slti
//    ArithLogI   001100   andi
//    ArithLogI   001101   ori
//    ArithLogI   001110   xori
//    Branch      000100   beq
//    Branch      000101   bne
//    BranchZ     000110   blez
//    BranchZ     000111   bgtz
//    LoadI       011000   llo
//    LoadI       011001   lhi
//    LoadStore   100000   lb
//    LoadStore   100001   lh
//    LoadStore   100011   lw
//    LoadStore   100100   lbu
//    LoadStore   100101   lhu
//    LoadStore   101000   sb
//    LoadStore   101001   sh
//    LoadStore   101011   sw

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
	i := Imm(bits & imm16Mask)
	fmt.Printf("s reg:  %05b\n", uint32(s))
	fmt.Printf("t reg:  %05b\n", uint32(t))
	fmt.Printf("imm16:  %016b\n", uint32(i))
	var args Args
	switch op {
	// ArithLogI
	case ADDI, ADDIU, SLTIU, SLTI, ANDI, ORI, XORI:
		args[0] = t
		args[1] = s
		args[2] = i
	// LoadI
	case LLO, LHI:
		args[0] = t
		args[1] = i
	// Branch
	case BEQ, BNE:
		args[0] = s
		args[1] = t
		args[2] = i
	// BranchZ
	case BLEZ, BGTZ:
		args[0] = t
		args[1] = s
		args[2] = i
	// LoadStore
	case LB, LH, LW, LBU, LHU, SB, SH, SW:
		args[0] = t
		m := Mem{
			Base:   s,
			Offset: int32(i), // TODO: Check convertion.
		}
		args[1] = m
	default:
		panic(fmt.Errorf("support for opcode %v not yet implemented", op))
	}
	return Inst{Op: op, Enc: bits, Args: args}, nil
}

// --- [ Instruction with jump encoding ] --------------------------------------

// Instructions with jump encoding:
//
//    syntax   opcode   op
//
//    Jump     000010   j
//    Jump     000011   jal
//    Trap     011010   trap

// decodeJumpInst decodes an instruction with jump encoding.
func decodeJumpInst(op Op, bits uint32) (Inst, error) {
	i := Imm(bits & imm26Mask)
	fmt.Printf("imm26:  %016b\n", uint32(i))
	// +-------------+-----------------+-----------+
	// | Syntax      | Template        | Encoding  |
	// +=============+=================+===========+
	// | Jump        | o label         | Jump      |
	// | Trap        | o i             | Jump      |
	// +-------------+-----------------+-----------+
	var args Args
	switch op {
	// Jump
	case J, JAL:
		args[0] = i
	// Trap
	case TRAP:
		args[0] = i
	default:
		panic(fmt.Errorf("support for opcode %v not yet implemented", op))
	}
	return Inst{Op: op, Enc: bits, Args: args}, nil
}

// ### [ Helper functions ] ####################################################

// opFromFunc maps from function bit pattern to MIPS opcode.
var opFromFunc = map[uint32]Op{
	0x00: SLL,   // 0b000000
	0x02: SRL,   // 0b000010
	0x03: SRA,   // 0b000011
	0x04: SLLV,  // 0b000100
	0x06: SRLV,  // 0b000110
	0x07: SRAV,  // 0b000111
	0x08: JR,    // 0b001000
	0x09: JALR,  // 0b001001
	0x10: MFHI,  // 0b010000
	0x11: MTHI,  // 0b010001
	0x12: MFLO,  // 0b010010
	0x13: MTLO,  // 0b010011
	0x18: MULT,  // 0b011000
	0x19: MULTU, // 0b011001
	0x1A: DIV,   // 0b011010
	0x1B: DIVU,  // 0b011011
	0x20: ADD,   // 0b100000
	0x21: ADDU,  // 0b100001
	0x22: SUB,   // 0b100010
	0x23: SUBU,  // 0b100011
	0x24: AND,   // 0b100100
	0x25: OR,    // 0b100101
	0x26: XOR,   // 0b100110
	0x27: NOR,   // 0b100111
	0x29: SLTU,  // 0b101001
	0x2A: SLT,   // 0b101010
}

// opFromOpcode maps from opcode bit pattern to MIPS opcode.
var opFromOpcode = map[uint32]Op{
	0x02: J,     // 0b000010
	0x03: JAL,   // 0b000011
	0x04: BEQ,   // 0b000100
	0x05: BNE,   // 0b000101
	0x06: BLEZ,  // 0b000110
	0x07: BGTZ,  // 0b000111
	0x08: ADDI,  // 0b001000
	0x09: ADDIU, // 0b001001
	//0x09: SLTIU, // 0b001001 // TODO: Check opcode; incorrect in mips_ref.pdf (duplicate of ADDIU opcode)
	0x0A: SLTI, // 0b001010
	0x0C: ANDI, // 0b001100
	0x0D: ORI,  // 0b001101
	0x0E: XORI, // 0b001110
	0x18: LLO,  // 0b011000
	0x19: LHI,  // 0b011001
	0x1A: TRAP, // 0b011010
	0x20: LB,   // 0b100000
	0x21: LH,   // 0b100001
	0x23: LW,   // 0b100011
	0x24: LBU,  // 0b100100
	0x25: LHU,  // 0b100101
	0x28: SB,   // 0b101000
	0x29: SH,   // 0b101001
	0x2B: SW,   // 0b101011
}
