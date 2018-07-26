// Package mips implements decoding of 32-bit MIPS I machine code.
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
	if len(args) > 0 {
		return fmt.Sprintf("%-*s%s", 8, i.Op, strings.Join(args, ", "))
	}
	return i.Op.String()
}

//go:generate stringer -linecomment -type Op

// An Op is a MIPS opcode.
type Op uint8

// Opcodes.
const (
	invalid Op = iota
	special    // register encoded instruction (function)
	bcond      // condition branch instruction (condition)

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
	LHU   // lhu
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
	XOR   // xor
	XORI  // xori

	// TODO: Figure out a good placement. Also, remove unused instructions above.
	// Furthermore, validate the instruction type and that decoding is done
	// correctly; e.g. I-type, R-type, J-type.
	BGEZ    // bgez
	BGEZAL  // bgezal
	BLTZAL  // bltzal
	BLTZ    // bltz
	LUI     // lui
	COP0    // cop0
	COP1    // cop1
	COP2    // cop2
	COP3    // cop3
	LWL     // lwl
	LWR     // lwr
	SWL     // swl
	SWR     // swr
	LWC0    // lwc0
	LWC1    // lwc1
	LWC2    // lwc2
	LWC3    // lwc3
	SWC0    // swc0
	SWC1    // swc1
	SWC2    // swc2
	SWC3    // swc3
	SYSCALL // syscall
	BREAK   // break

	// Co-Processor Operations
	// order matters: each instruction has a CO0 through CO3 variant.
	MFC0 // mfc0
	MFC1 // mfc1
	MFC2 // mfc2
	MFC3 // mfc3
	MTC0 // mtc0
	MTC1 // mtc1
	MTC2 // mtc2
	MTC3 // mtc3
	CFC0 // cfc0
	CFC1 // cfc1
	CFC2 // cfc2
	CFC3 // cfc3
	CTC0 // ctc0
	CTC1 // ctc1
	CTC2 // ctc2
	CTC3 // ctc3
	BCC0 // bcc0
	BCC1 // bcc1
	BCC2 // bcc2
	BCC3 // bcc3
	BC0F // bc0f
	BC1F // bc1f
	BC2F // bc2f
	BC3F // bc3f
	BC0T // bc0t
	BC1T // bc1t
	BC2T // bc2t
	BC3T // bc3t

	// CP0 Operations.
	TLBR  // tlbr
	TLBWI // tlbwi
	TLBWR // tlbwr
	TLBP  // tlbp
	RFE   // rfe
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
	// CPU General Registers.

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

	// TODO: Figure out what register number and syntax name to use for hi, lo
	// and pc registers.
	HI Reg = RA + 1 + iota // $hi
	LO                     // $lo
	PC                     // $pc

	// CP0 Special Registers
	// TODO: Figure out a better name for Co-processor (CP0) registers.

	// order matters; translating between CPU registers and COz registers.
	C0  // $0
	C1  // $1
	C2  // $2
	C3  // $3
	C4  // $4
	C5  // $5
	C6  // $6
	C7  // $7
	C8  // $8
	C9  // $9
	C10 // $10
	C11 // $11
	C12 // $12
	C13 // $13
	C14 // $14
	C15 // $15
	C16 // $16
	C17 // $17
	C18 // $18
	C19 // $19
	C20 // $20
	C21 // $21
	C22 // $22
	C23 // $23
	C24 // $24
	C25 // $25
	C26 // $26
	C27 // $27
	C28 // $28
	C29 // $29
	C30 // $30
	C31 // $31
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
	if m.Offset < 10 {
		return fmt.Sprintf("%d(%s)", m.Offset, m.Base)
	}
	return fmt.Sprintf("0x%X(%s)", m.Offset, m.Base)
}

// --- [ Integer constant ] ----------------------------------------------------

// An Imm is an integer constant.
type Imm struct {
	Imm     uint32
	Decimal bool
}

// String returns the string representation of the integer constant.
func (i Imm) String() string {
	// TODO: Change back from IDA format.
	if i.Decimal {
		return fmt.Sprintf("%d", uint32(i.Imm))
	}
	return fmt.Sprintf("0x%X", uint32(i.Imm))
}

// isArg ensures that only arguments can be assigned to the Arg interface.
func (Reg) isArg()   {}
func (PCRel) isArg() {}
func (Mem) isArg()   {}
func (Imm) isArg()   {}
