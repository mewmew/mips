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
		// TODO: remove IDA format.
		if len(args) >= 2 {
			if args[0] == args[1] {
				args = args[1:] // skip first argument if src and dest are identical.
			}
		}
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
	// TODO: consider renaming bcond to regimm.
	bcond // condition branch instruction (condition)

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

	// CP0 (System control co-processor) special registers.
	//
	// order matters; CO0 registers are directly followed by CO1, CO2 and CO3
	// registers.

	// TODO: Find names for remaining CO0 registers.

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
	// TODO: Add CO2 registers.

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

	// CO3 Registers.
	// TODO: Add CO3 registers.

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
