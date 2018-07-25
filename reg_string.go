// Code generated by "stringer -linecomment -type Reg"; DO NOT EDIT.

package mips

import "strconv"

const (
	_Reg_name_0 = "$zero$at$v0$v1$a0$a1$a2$a3$t0$t1$t2$t3$t4$t5$t6$t7$s0$s1$s2$s3$s4$s5$s6$s7$t8$t9$k0"
	_Reg_name_1 = "$gp$sp$fp$ra"
	_Reg_name_2 = "$hi$lo$pc$0$1$2$3$4$5$6$7$8$9$10$11$12$13$14$15$16$17$18$19$20$21$22$23$24$25$26$27$28$29$30$31"
)

var (
	_Reg_index_0 = [...]uint8{0, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50, 53, 56, 59, 62, 65, 68, 71, 74, 77, 80, 83}
	_Reg_index_1 = [...]uint8{0, 3, 6, 9, 12}
	_Reg_index_2 = [...]uint8{0, 3, 6, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 32, 35, 38, 41, 44, 47, 50, 53, 56, 59, 62, 65, 68, 71, 74, 77, 80, 83, 86, 89, 92, 95}
)

func (i Reg) String() string {
	switch {
	case 0 <= i && i <= 26:
		return _Reg_name_0[_Reg_index_0[i]:_Reg_index_0[i+1]]
	case 28 <= i && i <= 31:
		i -= 28
		return _Reg_name_1[_Reg_index_1[i]:_Reg_index_1[i+1]]
	case 64 <= i && i <= 98:
		i -= 64
		return _Reg_name_2[_Reg_index_2[i]:_Reg_index_2[i+1]]
	default:
		return "Reg(" + strconv.FormatInt(int64(i), 10) + ")"
	}
}
