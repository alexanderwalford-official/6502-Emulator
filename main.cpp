
/*
    A basic 6502 processor emulator written in C++ from watching a random youtube video.
    Re-Written by Alexander Walford.  
*/

#include <stdio.h>
#include <stdlib.h>

// keyword & byte value references
using byte = unsigned char;
using wrd = unsigned short;
using u32 unsigned int;

struct {
    static constexpr u32 max_memory = 1024 * 64; // max memory, change to suit your needs
    byte data [max_memory]; // data value for memory

    void init () { // reset all memory pointer values to 0
        for (u32 i = 0; i < max_memory, i++) {
            data[i] = 0;
        }
    }

    // read byte function
    byte & operator [] (u32 addr) const {
        // assert addr != 0;
        return data[addr];
    }

    // write 2 bytes
    void write_word (wrd val, u32 addr, u32 & cycles) {
        data[addr] = val & 0xFF;
        data[addr + 1] = (val >> 8);
        cycles -= 2;
    }
} memory;

struct {
    wrd program_cntr; // profram counter
    byte stack_pntr; // stack pointer
    byte A, X, Y; // registers

    // status flags starts
    byte C : 1; // carry
    byte Z : 1; // zero
    byte I : 1; // interupt disable
    byte D : 1; // decimal
    byte B : 1; // break cmd
    byte V : 1; // overflow
    byte N : 1; // negative
    // status flags end

    // get the next instruction
    byte fetch_byte (u32 & cycles, mem & memory) {
        byte data = memory[program_cntr];
        program_cntr++;
        cycles--;
        return data;
    }

    wrd fetch_word (u32 & cycles, mem & memory) {
        wrd data = memory[program_cntr];
        program_cntr++;

        data |= (memory[program_cntr] << 8);
        program_cntr++;

        cycles += 2;
        return data;
    }

    // read value from memory
    byte read_byte (u32 & cycles, byte addr, mem & memory) {
        byte data = memory[addr];
        cycles --;
        return data;
    }

    // operational codes
    static constexpr byte
        INS_LDA_IM = 0xA9, // instruct map
        INS_LDA_ZP = 0xA5, // zero page
        INS_LDA_ZPX = 0xB5,
        INS_JSR = 0x20; // jump to subroutine

    void set_lda_status () {
        Z = (A == 0);
        N = (A & 0b10000000) > 0;
    }

    // execute memory for x cycles
    void exec (u32 cycles, mem & memory) {
        while (cycles > 0) {
            byte instruct = fetch_byte(cycles, memory);
            switch (instruct) {
                case INS_LDA_IM:
                {
                    byte val = fetch_byte(cycles, memory);
                    A = val;
                    set_lda_status();
                } break;
                case INS_LDA_ZP:
                {
                    byte zpageaddr = fetch_byte(cycles, memory);
                    A = read_byte(cycles, zpageaddr, memory);
                    set_lda_status();
                } break;
                case INS_LDA_ZPX:
                {
                    byte zpageaddr = fetch_byte(cycles, memory);
                    zpageaddr += X;
                    cycles --;
                    A = read_byte(cycles, zpageaddr, memory);
                    set_lda_status();
                } break;
                case INS_JSR:
                {
                    wrd subaddr = fetch_word(cycles, memory);
                    memory.write_word(program_cntr - 1, stack_pntr, cycles);
                    program_cntr = subaddr;
                    cycles --;
                } break;
                default:
                {
                    // error
                    printf("The instruct was not handled %d");
                } break;
            }
        }
    }

    // execute reset cycles
    void reset (mem & memory) {
        program_cntr = 0xFFFC;
        stack_pntr = 0x0100;
        D = 0; // reset decimal status flag
        A = X = Y = 0; // perhaps reset all flags in future? may not represent original design
        memory.init(); // initialise the memory
    }
} central_processing_unit;

int main () {
    memory mem;
    central_processing_unit cpu;
    cpu.reset(mem);
    // example program
    mem[0xFFFC] = central_processing_unit :: INS_JSR;
    mem[0xFFFD] = 0x42;
    mem[0xFFFE] = 0x42;
    mem[0x4242] = central_processing_unit :: INS_LDA_IM;
    mem[0x03243] = 0x84;
    cpu.exec(9, mem); // refactor into program struct
    return 0;
}