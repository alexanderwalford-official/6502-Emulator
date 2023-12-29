#include <stdio.h>
#include <stdlib.h>
#include <vector>

// keyword & byte value references
using byte = unsigned char;
using wrd = unsigned short;
using u32 = unsigned int;

struct memory {
    static constexpr u32 max_memory = 1024 * 64; // max memory, change to suit your needs
    byte data[max_memory]; // data value for memory

    void init() { // reset all memory pointer values to 0
        for (u32 i = 0; i < max_memory; i++) {
            data[i] = 0;
        }
    }

    // read byte function
    byte & operator[](u32 addr) {
        // assert addr != 0;
        return data[addr];
    }

    // write 2 bytes
    void write_word(wrd val, u32 addr, u32& cycles) {
        data[addr] = val & 0xFF;
        data[addr + 1] = (val >> 8);
        cycles -= 2;
    }
};

struct status_flags {
    // status flags start
    byte C : 1; // carry
    byte Z : 1; // zero
    byte I : 1; // interrupt disable
    byte D : 1; // decimal
    byte B : 1; // break cmd
    byte V : 1; // overflow
    byte N : 1; // negative
    byte U : 1; // unused
    // status flags end
};

struct central_processing_unit {
    wrd program_cntr; // program counter
    byte stack_pntr; // stack pointer
    byte A, X, Y; // registers

    union {
        byte processor_status;
        status_flags flag;
    };

	// address values for process status flag bits
	static constexpr byte
		negative = 0b10000000,
		overflow = 0b01000000,
		break_bit = 0b000010000,
		unused = 0b000100000,
		interupt_disable = 0b000000100,
		zero = 0b00000001;

    // operational codes
    static constexpr byte
		// LDA
		INS_LDA_IM = 0xA9,
		INS_LDA_ZP = 0xA5, // zero page
		INS_LDA_ZPX = 0xB5, // zero page x
		INS_LDA_ABS = 0xAD,
		INS_LDA_ABSX = 0xBD,
		INS_LDA_ABSY = 0xB9,
		INS_LDA_INDX = 0xA1,
		INS_LDA_INDY = 0xB1,
		// LDX
		INS_LDX_IM = 0xA2,
		INS_LDX_ZP = 0xA6,
		INS_LDX_ZPY = 0xB6,
		INS_LDX_ABS = 0xAE,
		INS_LDX_ABSY = 0xBE,
		// LDY
		INS_LDY_IM = 0xA0,
		INS_LDY_ZP = 0xA4,
		INS_LDY_ZPX = 0xB4,
		INS_LDY_ABS = 0xAC,
		INS_LDY_ABSX = 0xBC,
		// STA
		INS_STA_ZP = 0x85,
		INS_STA_ZPX = 0x95,
		INS_STA_ABS = 0x8D,
		INS_STA_ABSX = 0x9D,
		INS_STA_ABSY = 0x99,
		INS_STA_INDX = 0x81,
		INS_STA_INDY = 0x91,
		// STX
		INS_STX_ZP = 0x86,
		INS_STX_ZPY = 0x96,
		INS_STX_ABS = 0x8E,
		// STY
		INS_STY_ZP = 0x84,
		INS_STY_ZPX = 0x94,
		INS_STY_ABS = 0x8C,

		INS_TSX = 0xBA,
		INS_TXS = 0x9A,
		INS_PHA = 0x48,
		INS_PLA = 0x68,
		INS_PHP = 0x08,
		INS_PLP = 0x28,

		INS_JMP_ABS = 0x4C,
		INS_JMP_IND = 0x6C,
		INS_JSR = 0x20,
		INS_RTS = 0x60,

		// logical operations (gates)

		// AND
		INS_AND_IM = 0x29,
		INS_AND_ZP = 0x25,
		INS_AND_ZPX = 0x35,
		INS_AND_ABS = 0x2D,
		INS_AND_ABSX = 0x3D,
		INS_AND_ABSY = 0x39,
		INS_AND_INDX = 0x21,
		INS_AND_INDY = 0x31,

		// OR
		INS_ORA_IM = 0x09,
		INS_ORA_ZP = 0x05,
		INS_ORA_ZPX = 0x15,
		INS_ORA_ABS = 0x0D,
		INS_ORA_ABSX = 0x1D,
		INS_ORA_ABSY = 0x19,
		INS_ORA_INDX = 0x01,
		INS_ORA_INDY = 0x11,

		// EOR
		INS_EOR_IM = 0x49,
		INS_EOR_ZP = 0x45,
		INS_EOR_ZPX = 0x55,
		INS_EOR_ABS = 0x4D,
		INS_EOR_ABSX = 0x5D,
		INS_EOR_ABSY = 0x59,
		INS_EOR_INDX = 0x41,
		INS_EOR_INDY = 0x51,

		// BIT
		INS_BIT_ZP = 0x24,
		INS_BIT_ABS = 0x2C,

		// transfer registers
		INS_TAX = 0xAA,
		INS_TAY = 0xA8,
		INS_TXA = 0x8A,
		INS_TYA = 0x98,

		// increments, decrements
		INS_INX = 0xE8,
		INS_INY = 0xC8,
		INS_DEY = 0x88,
		INS_DEX = 0xCA,
		INS_DEC_ZP = 0xC6,
		INS_DEC_ZPX = 0xD6,
		INS_DEC_ABS = 0xCE,
		INS_DEC_ABSX = 0xDE,
		INS_INC_ZP = 0xE6,
		INS_INC_ZPX = 0xF6,
		INS_INC_ABS = 0xEE,
		INS_INC_ABSX = 0xFE,

		// branches
		INS_BEQ = 0xF0,
		INS_BNE = 0xD0,
		INS_BCS = 0xB0,
		INS_BCC = 0x90,
		INS_BMI = 0x30,
		INS_BPL = 0x10,
		INS_BVC = 0x50,
		INS_BVS = 0x70,

		// status flag changes
		INS_CLC = 0x18,
		INS_SEC = 0x38,
		INS_CLD = 0xD8,
		INS_SED = 0xF8,
		INS_CLI = 0x58,
		INS_SEI = 0x78,
		INS_CLV = 0xB8,

		// arithmetic
		INS_ADC = 0x69,
		INS_ADC_ZP = 0x65,
		INS_ADC_ZPX = 0x75,
		INS_ADC_ABS = 0x6D,
		INS_ADC_ABSX = 0x7D,
		INS_ADC_ABSY = 0x79,
		INS_ADC_INDX = 0x61,
		INS_ADC_INDY = 0x71,

		INS_SBC = 0xE9,
		INS_SBC_ABS = 0xED,
		INS_SBC_ZP = 0xE5,
		INS_SBC_ZPX = 0xF5,
		INS_SBC_ABSX = 0xFD,
		INS_SBC_ABSY = 0xF9,
		INS_SBC_INDX = 0xE1,
		INS_SBC_INDY = 0xF1,

		// register comparison
		INS_CMP = 0xC9,
		INS_CMP_ZP = 0xC5,
		INS_CMP_ZPX = 0xD5,
		INS_CMP_ABS = 0xCD,
		INS_CMP_ABSX = 0xDD,
		INS_CMP_ABSY = 0xD9,
		INS_CMP_INDX = 0xC1,
		INS_CMP_INDY = 0xD1,

		INS_CPX = 0xE0,
		INS_CPY = 0xC0,
		INS_CPX_ZP = 0xE4,
		INS_CPY_ZP = 0xC4,
		INS_CPX_ABS = 0xEC,
		INS_CPY_ABS = 0xCC,

		// shifts
		INS_ASL = 0x0A,
		INS_ASL_ZP = 0x06,
		INS_ASL_ZPX = 0x16,
		INS_ASL_ABS = 0x0E,
		INS_ASL_ABSX = 0x1E,

		INS_LSR = 0x4A,
		INS_LSR_ZP = 0x46,
		INS_LSR_ZPX = 0x56,
		INS_LSR_ABS = 0x4E,
		INS_LSR_ABSX = 0x5E,

		INS_ROL = 0x2A,
		INS_ROL_ZP = 0x26,
		INS_ROL_ZPX = 0x36,
		INS_ROL_ABS = 0x2E,
		INS_ROL_ABSX = 0x3E,

		INS_ROR = 0x6A,
		INS_ROR_ZP = 0x66,
		INS_ROR_ZPX = 0x76,
		INS_ROR_ABS = 0x6E,
		INS_ROR_ABSX = 0x7E,

		// misc
		INS_NOP = 0xEA, // no operation
		INS_BRK = 0x00, // break
		INS_RTI = 0x40;

	// set the process status after loading register instruction
	void set_zero_negative_flags(byte reg) {
		flag.Z = (reg == 0);
		flag.N = (reg & negative) > 0;
	}

	// return the memory address of the stack pointer, 16 bit address
	wrd stack_pointer_to_address () const {
		return 0x100 | stack_pntr;
	}

	// write a byte to a specific memory address
	void write_byte(byte val, u32 cycles, wrd addr, memory& memory) {
		memory[addr] = val;
		cycles--;
	}

	// push word value to process stack, uses stack pointer
	void push_word_to_stack(u32& cycles, memory& memory, wrd val) {
		write_byte(val >> 8, cycles, stack_pointer_to_address(), memory);
		stack_pntr--;
		write_byte(val & 0xFF, cycles, stack_pointer_to_address(), memory);
		stack_pntr--;
	}

    // get the next instruction
    byte fetch_byte(u32& cycles, memory& memory) {
        byte data = memory[program_cntr];
        program_cntr++;
        cycles--;
        return data;
    }

	// fetch word value from memory
    wrd fetch_word(u32& cycles, memory& memory) {
        wrd data = 0;
        data |= memory[program_cntr];
        program_cntr++;
        data |= (memory[program_cntr] << 8);
        program_cntr++;
        cycles += 2;
        return data;
    }

    // read byte value from memory
    byte read_byte(u32& cycles, u32 addr, memory& memory) {
        byte data = memory[addr];
        cycles--;
        return data;
    }

	// read the word value from memory at the specified address
	wrd read_word(u32& cycles, wrd addr, memory& mem) {
		byte low_byte = read_byte(cycles, addr, mem);
		byte high_byte = read_byte(cycles, addr + 1, mem);
		return low_byte | (high_byte << 8);
	}

	// remove the byte from the stack, uses stack pointer
	byte pop_byte_from_stack(u32 cycles, memory& memory) {
		stack_pntr++;
		cycles--;
		const wrd stack_pointer_word = stack_pointer_to_address();
		byte val = memory[stack_pointer_word];
		cycles--;
		return val;
	}

	// pop word value from stack, uses stack pointer and read word methods, takes 1 cycles but 2 pointers
	wrd pop_word_from_stack(u32 cycles, memory& memory) {
		wrd val_from_stack = read_word(cycles, stack_pointer_to_address() + 1, memory);
		stack_pntr += 2;
		cycles--;
		return val_from_stack;
	}

	// sets the zero page flag and negative flag values
    void set_lda_status() {
        flag.Z = (A == 0);
        flag.N = (A & 0b10000000) > 0;
    }

    // execute memory for x cycles
    void exec(u32 cycles, memory& memory) {
        while (cycles > 0) {
            byte instruct = fetch_byte(cycles, memory);
            switch (instruct) {
            case INS_LDA_IM: {
                byte val = fetch_byte(cycles, memory);
                A = val;
                set_lda_status();
            } break;
            case INS_LDA_ZP: {
                byte zpageaddr = fetch_byte(cycles, memory);
                A = read_byte(cycles, zpageaddr, memory);
                set_lda_status();
            } break;
            case INS_LDA_ZPX: {
                byte zpageaddr = fetch_byte(cycles, memory);
                zpageaddr += X;
                cycles--;
                A = read_byte(cycles, zpageaddr, memory);
                set_lda_status();
            } break;
            case INS_JSR: {
                wrd subaddr = fetch_word(cycles, memory);
                memory.write_word(program_cntr, stack_pntr - 2, cycles);
                program_cntr = subaddr;
                cycles--;
            } break;
            default: {
                // error
                printf("The instruction %02X was not handled\n", instruct);
            } break;
            }
        }
    }

    // execute reset cycles
    void reset(wrd reset_vector, memory& memory) {
        program_cntr = reset_vector;
        stack_pntr = 0xFF;
		flag.C = flag.Z = flag.I = flag.D = flag.B = flag.V = flag.N = 0; // perhaps reset all flags in the future? may not represent the original design
		A = X = Y = 0;
        memory.init(); // initialize the memory
    }
};

struct application {
	memory mem;

	// function to initialize memory with instructions
	void initialise(int jsrArg1, int jsrArg2, int jsrArg3, int ldaArg1, int ldaArg2) {
		// set up memory based on arguments
		mem[0xFFFC] = central_processing_unit::INS_JSR;
		mem[0xFFFD] = jsrArg1;
		mem[0xFFFE] = jsrArg2;
		mem[0x4242] = central_processing_unit::INS_LDA_IM;
		mem[0x03243] = ldaArg1;
	}

	// get the size of the instructions dynamically
	int get_size() const {
		return memory::max_memory;
	}
};

int main() {
	// required objects
    memory mem;
    central_processing_unit CPU;

	// reset CPU
    CPU.reset(0xFFFC, mem);

	// create instances of Program
	application app;

	// initialise memory with specific arguments for instructions
	app.initialise(0x42, 0x42, 0x42, 0x84, 0x00);

	// get the number of required cycles for the application
	u32 cycles = 0;
	for (int i = 0; i < app.get_size(); ++i) {
		cycles += 1;
	}

	// execute instructions with cycles based on instruction size dynamically
	CPU.exec(cycles, app.mem);

    return 0;
}