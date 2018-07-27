package mips

import (
	"encoding/binary"
	"testing"
)

func TestDecode(t *testing.T) {
	golden := []struct {
		in   uint32
		want string
	}{
		// Syntactic suger.
		{in: 0x00000000, want: "nop"},
		// Load/Store Instructions
		{in: 0x80850002, want: "lb      $a1, 2($a0)"},
		{in: 0x8085FFFE, want: "lb      $a1, -2($a0)"},
		{in: 0x90AA000F, want: "lbu     $t2, 15($a1)"},
		{in: 0x90AA0010, want: "lbu     $t2, 0x10($a1)"},
		/*
			{in: 0x00000000, want: "LH      $t, offset($s)"},
			{in: 0x00000000, want: "LHU     $t, offset($s)"},
			{in: 0x00000000, want: "LW      $t, offset($s)"},
			{in: 0x00000000, want: "LWL     $t, offset($s)"},
			{in: 0x00000000, want: "LWR     $t, offset($s)"},
			{in: 0x00000000, want: "SB      $t, offset($s)"},
			{in: 0x00000000, want: "SH      $t, offset($s)"},
			{in: 0x00000000, want: "SW      $t, offset($s)"},
			{in: 0x00000000, want: "SWL     $t, offset($s)"},
			{in: 0x00000000, want: "SWR     $t, offset($s)"},
			// Arithmetic Instructions (ALU Immediate)
			{in: 0x00000000, want: "ADDI    $t, $s, immediate"},
			{in: 0x00000000, want: "ADDIU   $t, $s, immediate"},
			{in: 0x00000000, want: "SLTI    $t, $s, immediate"},
			{in: 0x00000000, want: "SLTIU   $t, $s, immediate"},
			{in: 0x00000000, want: "ANDI    $t, $s, immediate"},
			{in: 0x00000000, want: "ORI     $t, $s, immediate"},
			{in: 0x00000000, want: "XORI    $t, $s, immediate"},
			{in: 0x00000000, want: "LUI     $t, immediate"},
			// Arithmetic Instructions (3-operand, register-type)
			{in: 0x00000000, want: "ADD     $d, $s, $t"},
			{in: 0x00000000, want: "ADDU    $d, $s, $t"},
			{in: 0x00000000, want: "SUB     $d, $s, $t"},
			{in: 0x00000000, want: "SUBU    $d, $s, $t"},
			{in: 0x00000000, want: "SLT     $d, $s, $t"},
			{in: 0x00000000, want: "SLTU    $d, $s, $t"},
			{in: 0x00000000, want: "AND     $d, $s, $t"},
			{in: 0x00000000, want: "OR      $d, $s, $t"},
			{in: 0x00000000, want: "XOR     $d, $s, $t"},
			{in: 0x00000000, want: "NOR     $d, $s, $t"},
			// Shift Instructions
			{in: 0x00000000, want: "SLL     $d, $t, shift"},
			{in: 0x00000000, want: "SRL     $d, $t, shift"},
			{in: 0x00000000, want: "SRA     $d, $t, shift"},
			{in: 0x00000000, want: "SLLV    $d, $t, $s"},
			{in: 0x00000000, want: "SRLV    $d, $t, $s"},
			{in: 0x00000000, want: "SRAV    $d, $t, $s"},
			// Multiply/Divide Instructions
			{in: 0x00000000, want: "MULT    $s, $t"},
			{in: 0x00000000, want: "MULTU   $s, $t"},
			{in: 0x00000000, want: "DIV     $s, $t"},
			{in: 0x00000000, want: "DIVU    $s, $t"},
			{in: 0x00000000, want: "MFHI    $d"},
			{in: 0x00000000, want: "MTHI    $s"},
			{in: 0x00000000, want: "MFLO    $d"},
			{in: 0x00000000, want: "MTLO    $s"},
			// Jump and Branch Instructions
			{in: 0x00000000, want: "J       target"},
			{in: 0x00000000, want: "JAL     target"},
			{in: 0x00000000, want: "JR      $s"},
			{in: 0x00000000, want: "JALR    $s, $d"},
			{in: 0x00000000, want: "BEQ     $s, $t, offset"},
			{in: 0x00000000, want: "BNE     $s, $t, offset"},
			{in: 0x00000000, want: "BLEZ    $s, offset"},
			{in: 0x00000000, want: "BGTZ    $s, offset"},
			{in: 0x00000000, want: "BLTZ    $s, offset"},
			{in: 0x00000000, want: "BGEZ    $s, offset"},
			{in: 0x00000000, want: "BLTZAL  $s, offset"},
			{in: 0x00000000, want: "BGEZAL  $s, offset"},
			// Special Instructions
			{in: 0x00000000, want: "SYSCALL code"},
			{in: 0x00000000, want: "BREAK   code"},
			// Coprocessor Instructions
			{in: 0x00000000, want: "LWCz     $t, offset($s)"},
			{in: 0x00000000, want: "SWCz     $t, offset($s)"},
			{in: 0x00000000, want: "MTCz     $t, $d"},
			{in: 0x00000000, want: "MFCz     $t, $d"},
			{in: 0x00000000, want: "CTCz     $t, $d"},
			{in: 0x00000000, want: "CFCz     $t, $d"},
			{in: 0x00000000, want: "COPz     cofunc"},
			{in: 0x00000000, want: "BCzT     offset"},
			{in: 0x00000000, want: "BCzF     offset"},
			// System Control Coprocessor (CP0) Instructions
			{in: 0x00000000, want: "MTC0 $t, $d"},
			{in: 0x00000000, want: "MFC0 $t, $d"},
			{in: 0x00000000, want: "TLBR"},
			{in: 0x00000000, want: "TLBWI"},
			{in: 0x00000000, want: "TLBWR"},
			{in: 0x00000000, want: "TLBP"},
			{in: 0x00000000, want: "RFE"},
		*/
	}
	for _, g := range golden {
		var src [4]byte
		binary.LittleEndian.PutUint32(src[:], g.in)
		inst, err := Decode(src[:])
		if err != nil {
			t.Errorf("decoding error; %v", err)
			continue
		}
		got := inst.String()
		if g.want != got {
			t.Errorf("instruction mismatch; expected `%v`, got `%v`", g.want, got)
			continue
		}
		//fmt.Println("PASS:", got)
	}
}
