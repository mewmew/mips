// Package mips implements decoding of 32-bit MIPS machine code.
package mips

import (
	"fmt"
	"strings"
)

// An Inst is a single instruction.
type Inst struct {
	// Opcode mnemonic
	Op Op
	// Raw encoding bits.
	Enc uint32
	// Instruction arguments, in MIPS manual order.
	Args Args
}

// String returns the string representation of the instruction.
func (i Inst) String() string {
	if i.Enc == 0 {
		// sll $zero, $zero, 0
		return "nop"
	}
	var args []string
	for _, arg := range i.Args {
		if arg == nil {
			break
		}
		args = append(args, arg.String())
	}
	return fmt.Sprintf("%s %s", i.Op, strings.Join(args, ", "))
}

//go:generate stringer -linecomment -type Op

// An Op is a MIPS opcode.
type Op uint8

// Opcodes.
const (
	_ Op = iota

	ADD   // add
	ADDI  // addi
	ADDIU // addiu
	ADDU  // addu
	AND   // and
	ANDI  // andi
	BEQ   // beq
	BGTZ  // bgtz
	BLEZ  // blez
	BNE   // bne
	DIV   // div
	DIVU  // divu
	J     // j
	JAL   // jal
	JALR  // jalr
	JR    // jr
	LB    // lb
	LBU   // lbu
	LH    // lh
	LHI   // lhi
	LHU   // lhu
	LLO   // llo
	LW    // lw
	MFHI  // mfhi
	MFLO  // mflo
	MTHI  // mthi
	MTLO  // mtlo
	MULT  // mult
	MULTU // multu
	NOR   // nor
	OR    // or
	ORI   // ori
	SB    // sb
	SH    // sh
	SLL   // sll
	SLLV  // sllv
	SLT   // slt
	SLTI  // slti
	SLTIU // sltiu
	SLTU  // sltu
	SRA   // sra
	SRAV  // srav
	SRL   // srl
	SRLV  // srlv
	SUB   // sub
	SUBU  // subu
	SW    // sw
	TRAP  // trap
	XOR   // xor
	XORI  // xori
)

// An Args holds the instruction arguments. If an instruction has fewer than 3
// arguments, the final elements in the array are nil.
type Args [3]Arg

// An Arg is a single instruction argument, one of these types: Reg, PCRel, Mem,
// Imm.
type Arg interface {
	fmt.Stringer
	// isArg ensures that only arguments can be assigned to the Arg interface.
	isArg()
}

// --- [ Register ] ------------------------------------------------------------

//go:generate stringer -linecomment -type Reg

// A Reg is a single register.
// The zero value denotes $zero, not the absence of a register.
type Reg uint8

// Registers.
const (
	ZERO Reg = 0  // $zero
	AT   Reg = 1  // $at
	V0   Reg = 2  // $v0
	V1   Reg = 3  // $v1
	A0   Reg = 4  // $a0
	A1   Reg = 5  // $a1
	A2   Reg = 6  // $a2
	A3   Reg = 7  // $a3
	T0   Reg = 8  // $t0
	T1   Reg = 9  // $t1
	T2   Reg = 10 // $t2
	T3   Reg = 11 // $t3
	T4   Reg = 12 // $t4
	T5   Reg = 13 // $t5
	T6   Reg = 14 // $t6
	T7   Reg = 15 // $t7
	S0   Reg = 16 // $s0
	S1   Reg = 17 // $s1
	S2   Reg = 18 // $s2
	S3   Reg = 19 // $s3
	S4   Reg = 20 // $s4
	S5   Reg = 21 // $s5
	S6   Reg = 22 // $s6
	S7   Reg = 23 // $s7
	T8   Reg = 24 // $t8
	T9   Reg = 25 // $t9
	K0   Reg = 26 // $k0
	K1   Reg = 26 // $k1
	GP   Reg = 28 // $gp
	SP   Reg = 29 // $sp
	FP   Reg = 30 // $fp
	RA   Reg = 31 // $ra
)

// --- [ PC relative memory address ] ------------------------------------------

// A PCRel describes a memory address (usually a code label) as a distance
// relative to the program counter.
type PCRel int32

// String returns the string representation of the PC relative memory address.
func (r PCRel) String() string {
	return fmt.Sprintf("%d", r)
}

// --- [ Memory reference ] ----------------------------------------------------

// A Mem is a memory reference made up of a base R and immediate X. The
// effective memory address is R+X.
type Mem struct {
	Base   Reg
	Offset int32
}

// String returns the string representation of the memory reference.
func (m Mem) String() string {
	return fmt.Sprintf("%d(%s)", m.Offset, m.Base)
}

// --- [ Integer constant ] ----------------------------------------------------

// An Imm is an integer constant.
type Imm uint32

// String returns the string representation of the integer constant.
func (i Imm) String() string {
	return fmt.Sprintf("%d", uint32(i))
}

// isArg ensures that only arguments can be assigned to the Arg interface.
func (Reg) isArg()   {}
func (PCRel) isArg() {}
func (Mem) isArg()   {}
func (Imm) isArg()   {}
