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
	bc         // branch on co-processor (bccond)

	// Load/Store Instructions
	LB  // lb
	LBU // lbu
	LH  // lh
	LHU // lhu
	LW  // lw
	LWL // lwl
	LWR // lwr
	SB  // sb
	SH  // sh
	SW  // sw
	SWL // swl
	SWR // swr

	// Arithmetic Instructions (ALU Immediate)
	ADDI  // addi
	ADDIU // addiu
	SLTI  // slti
	SLTIU // sltiu
	ANDI  // andi
	ORI   // ori
	XORI  // xori
	LUI   // lui

	// Arithmetic Instructions (3-operand, register-type)
	ADD  // add
	ADDU // addu
	SUB  // sub
	SUBU // subu
	SLT  // slt
	SLTU // sltu
	AND  // and
	OR   // or
	XOR  // xor
	NOR  // nor

	// Shift Instructions
	SLL  // sll
	SRL  // srl
	SRA  // sra
	SLLV // sllv
	SRLV // srlv
	SRAV // srav

	// Multiply/Divide Instructions
	MULT  // mult
	MULTU // multu
	DIV   // div
	DIVU  // divu
	MFHI  // mfhi
	MTHI  // mthi
	MFLO  // mflo
	MTLO  // mtlo

	// Jump and Branch Instructions
	J      // j
	JAL    // jal
	JR     // jr
	JALR   // jalr
	BEQ    // beq
	BNE    // bne
	BLEZ   // blez
	BGTZ   // bgtz
	BLTZ   // bltz
	BGEZ   // bgez
	BLTZAL // bltzal
	BGEZAL // bgezal

	// Special Instructions
	SYSCALL // syscall
	BREAK   // break

	// System Control Coprocessor (CP0) Instructions
	TLBR  // tlbr
	TLBWI // tlbwi
	TLBWR // tlbwr
	TLBP  // tlbp
	RFE   // rfe

	// Coprocessor Instructions
	// order matters: each instruction has a CO0 through CO3 variant.
	LWC0 // lwc0
	LWC1 // lwc1
	LWC2 // lwc2
	LWC3 // lwc3
	SWC0 // swc0
	SWC1 // swc1
	SWC2 // swc2
	SWC3 // swc3
	MTC0 // mtc0
	MTC1 // mtc1
	MTC2 // mtc2
	MTC3 // mtc3
	MFC0 // mfc0
	MFC1 // mfc1
	MFC2 // mfc2
	MFC3 // mfc3
	CTC0 // ctc0
	CTC1 // ctc1
	CTC2 // ctc2
	CTC3 // ctc3
	CFC0 // cfc0
	CFC1 // cfc1
	CFC2 // cfc2
	CFC3 // cfc3
	COP0 // cop0
	COP1 // cop1
	COP2 // cop2
	COP3 // cop3
	BC0T // bc0t
	BC1T // bc1t
	BC2T // bc2t
	BC3T // bc3t
	BC0F // bc0f
	BC1F // bc1f
	BC2F // bc2f
	BC3F // bc3f
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

// A Reg is a single register. The zero value denotes $zero, not the absence of
// a register.
type Reg uint8

// Registers.
const (
	// CPU general-purpose registers.
	ZERO Reg = iota // $zero
	AT              // $at
	V0              // $v0
	V1              // $v1
	A0              // $a0
	A1              // $a1
	A2              // $a2
	A3              // $a3
	T0              // $t0
	T1              // $t1
	T2              // $t2
	T3              // $t3
	T4              // $t4
	T5              // $t5
	T6              // $t6
	T7              // $t7
	S0              // $s0
	S1              // $s1
	S2              // $s2
	S3              // $s3
	S4              // $s4
	S5              // $s5
	S6              // $s6
	S7              // $s7
	T8              // $t8
	T9              // $t9
	K0              // $k0
	K1              // $k1
	GP              // $gp
	SP              // $sp
	FP              // $fp
	RA              // $ra

	PC // $pc

	// Integer multiply unit registers.
	HI // $hi
	LO // $lo

	// order matters; CO0 registers are directly followed by CO1, CO2 and CO3
	// registers.

	// CP0 (System control co-processor) special registers.
	// TODO: Add names for remaining CO0 registers.
	CP0Reg0
	CP0Reg1
	BusCtrl // BusCtrl
	Config  // Config
	CP0Reg4
	CP0Reg5
	CP0Reg6
	CP0Reg7
	BadVaddr // BadVaddr
	Count    // Count
	PortSize // PortSize
	Compare  // Compare
	SR       // SR
	Cause    // Cause
	EPC      // EPC
	PRId     // PRId
	CP0Reg16
	CP0Reg17
	CP0Reg18
	CP0Reg19
	CP0Reg20
	CP0Reg21
	CP0Reg22
	CP0Reg23
	CP0Reg24
	CP0Reg25
	CP0Reg26
	CP0Reg27
	CP0Reg28
	CP0Reg29
	CP0Reg30
	CP0Reg31

	// CP1 (FPU) registers.
	F0  // $f0
	F1  // $f1
	F2  // $f2
	F3  // $f3
	F4  // $f4
	F5  // $f5
	F6  // $f6
	F7  // $f7
	F8  // $f8
	F9  // $f9
	F10 // $f10
	F11 // $f11
	F12 // $f12
	F13 // $f13
	F14 // $f14
	F15 // $f15
	F16 // $f16
	F17 // $f17
	F18 // $f18
	F19 // $f19
	F20 // $f20
	F21 // $f21
	F22 // $f22
	F23 // $f23
	F24 // $f24
	F25 // $f25
	F26 // $f26
	F27 // $f27
	F28 // $f28
	F29 // $f29
	F30 // $f30
	F31 // $f31

	// CO2 Registers.
	CO2Reg0  // $0
	CO2Reg1  // $1
	CO2Reg2  // $2
	CO2Reg3  // $3
	CO2Reg4  // $4
	CO2Reg5  // $5
	CO2Reg6  // $6
	CO2Reg7  // $7
	CO2Reg8  // $8
	CO2Reg9  // $9
	CO2Reg10 // $10
	CO2Reg11 // $11
	CO2Reg12 // $12
	CO2Reg13 // $13
	CO2Reg14 // $14
	CO2Reg15 // $15
	CO2Reg16 // $16
	CO2Reg17 // $17
	CO2Reg18 // $18
	CO2Reg19 // $19
	CO2Reg20 // $20
	CO2Reg21 // $21
	CO2Reg22 // $22
	CO2Reg23 // $23
	CO2Reg24 // $24
	CO2Reg25 // $25
	CO2Reg26 // $26
	CO2Reg27 // $27
	CO2Reg28 // $28
	CO2Reg29 // $29
	CO2Reg30 // $30
	CO2Reg31 // $31

	// CO3 Registers.
	// TODO: Add names for CO3 registers.
	CP3Reg0  // $cp3_0
	CP3Reg1  // $cp3_1
	CP3Reg2  // $cp3_2
	CP3Reg3  // $cp3_3
	CP3Reg4  // $cp3_4
	CP3Reg5  // $cp3_5
	CP3Reg6  // $cp3_6
	CP3Reg7  // $cp3_7
	CP3Reg8  // $cp3_8
	CP3Reg9  // $cp3_9
	CP3Reg10 // $cp3_10
	CP3Reg11 // $cp3_11
	CP3Reg12 // $cp3_12
	CP3Reg13 // $cp3_13
	CP3Reg14 // $cp3_14
	CP3Reg15 // $cp3_15
	CP3Reg16 // $cp3_16
	CP3Reg17 // $cp3_17
	CP3Reg18 // $cp3_18
	CP3Reg19 // $cp3_19
	CP3Reg20 // $cp3_20
	CP3Reg21 // $cp3_21
	CP3Reg22 // $cp3_22
	CP3Reg23 // $cp3_23
	CP3Reg24 // $cp3_24
	CP3Reg25 // $cp3_25
	CP3Reg26 // $cp3_26
	CP3Reg27 // $cp3_27
	CP3Reg28 // $cp3_28
	CP3Reg29 // $cp3_29
	CP3Reg30 // $cp3_30
	CP3Reg31 // $cp3_31
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
	// TODO: Figure out what format to use for memory offsets.
	if m.Offset < 0x10 {
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
		return fmt.Sprintf("%d", i.Imm)
	}
	return fmt.Sprintf("0x%X", i.Imm)
}

// isArg ensures that only arguments can be assigned to the Arg interface.
func (Reg) isArg()   {}
func (PCRel) isArg() {}
func (Mem) isArg()   {}
func (Imm) isArg()   {}
