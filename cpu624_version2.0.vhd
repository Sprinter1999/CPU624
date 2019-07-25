library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity CPU624 is
    port (
				--***************************INPUT SIGNAL*************************--
        SWC, SWB, SWA : in std_logic;
				--模式开关值
		  CLR: IN STD_LOGIC;
				--复位信号，低电平有效，运行时先摁下clr
		  C, Z:IN STD_LOGIC;
				--Flag,标志寄存器，保存进位信号C和得零信号Z
		  W1, W2, W3, T3, QD : in std_logic;
				--  FIRST MACHINE-CIRCLE,SECOND ONE(SHORT ENABLED THEN NOT AVAILABLE),THIRD ONE(LONG ENABLED TO ENTER)
				-- T3 定界，T3下降沿可
				-- QD：Wi的节拍电位产生器
        IR : in std_logic_vector(3 downto 0);
				-- INSTRUCTION
				
				--**************************OUTPUT SIGNAL*************************--
        LPC, CIN: out std_logic;
				-- 载入 PC and 控制低位进位输入
		  PCADD:out std_logic;
				-- 用于加“偏移量”生成新的PC地址
		  ARINC,PCINC:OUT STD_LOGIC;
				-- AR++，PC++
		  STOP:out std_LOGIC;
				-- 停止产生时钟脉冲
		  LIR, LAR:out std_logic;
				-- 字面意思，load IR or AR
		  LONG,SHORT:OUT STD_LOGIC;
				--控制产生额外的W3和控制只产生W1
		  SBUS,ABUS,MBUS:OUT STD_LOGIC;
				--总线控制权使能
		  DRW,MEMW: OUT STD_LOGIC;
				-- 写寄存器使能,和写存储器使能
        SEL3, SEL2, SEL1, SEL0: OUT STD_LOGIC;
				--片选，模式选择，比如R0R1||R2R3
		  SELCTL: OUT STD_LOGIC;
				--进入控制台模式
		  LDC, LDZ: OUT STD_LOGIC;
				--	修改标志寄存器C，Z
		  S : out std_logic_vector(3 downto 0);
				--  ALU运算模式,详情可见教材P48页
		  M : out std_logic
				-- ALU 运算模式，控制算术运算还是逻辑运算
		  
    );
end CPU624;


architecture struct of cpu624 is
	signal SWCBA:std_logic_vector(2 downto 0);
	signal ST0,SST0,FLAG,CFLAG:Std_logic;
	
	--st0是标志符号，用于区分同一个控制台模式的不同阶段
			--sst0用于修改st0
begin
	process(T3,CLR)
		begin
		IF(CLR='0') THEN
				FLAG<='0';
		
		elsiF(T3'EVENT AND T3='1') THEN
		
				IF(CFLAG='1') THEN
					FLAG<=NOT FLAG;
				END IF;
			END IF;
		END PROCESS;
		
	PROCESS(T3,CLR)
		BEGIN
		IF(CLR='0') THEN
				ST0<='0';
		
		elsIF(T3'EVENT AND T3='0') THEN
				IF(SST0='1') THEN
					ST0<='1';
				END IF;
			END IF;
		END PROCESS;
	
	PROCESS(SWA,SWB,SWC,W1,W2,W3,ST0,C,Z,CLR)
	variable ADD:std_logic;
	variable SUB:std_logic;
	variable ANDD:std_logic;
	variable INC:std_logic;
	variable LD:std_logic;
	variable ST:std_logic;
	variable JC:std_logic;
	variable JZ:std_logic;
	variable JMP:std_logic;
	variable OUTT:std_logic;
	variable XORR:std_logic;
	variable ORR:std_logic;
	variable NOTT:std_logic;
	variable STP:std_logic;
	variable NOP:std_logic;
	variable RM:std_logic;
	variable WM:std_logic;
	variable RR:std_logic;
	variable WR:std_logic;
	variable FE:std_logic;
	variable FEP:std_logic;
	variable HI,HII,HIV,HV:STD_LOGIC;
	BEGIN
	ADD:=(NOT IR(3))AND(NOT IR(2))AND(NOT IR(1))AND (IR(0));
	SUB:=(NOT IR(3))AND(NOT IR(2))AND(IR(1))AND (NOT IR((0)));
	ANDD:=(NOT IR(3))AND(NOT IR(2))AND(IR(1))AND (IR(0));
	INC:=(NOT IR(3))AND(IR(2))AND(NOT IR(1))AND (NOT IR(0));
	LD:=(NOT IR(3))AND(IR(2))AND(NOT IR(1))AND (IR(0));
	ST:=(NOT IR(3))AND(IR(2))AND(IR(1))AND (NOT IR(0));
	JC:=(NOT IR(3))AND(IR(2))AND(IR(1))AND (IR(0));
	JZ:=(IR(3))AND(NOT IR(2))AND(NOT IR(1))AND (NOT IR(0));
	JMP:=(IR(3))AND(NOT IR(2))AND(NOT IR(1))AND (IR(0));
	STP:=(IR(3))AND(IR(2))AND(IR(1))AND (NOT IR(0));
	NOTT:=(IR(3))AND(IR(2))AND(NOT IR(1))AND (IR(0));
	NOP:=(NOT IR(3))AND(NOT IR(2))AND(NOT IR(1))AND (NOT IR(0));
	ORR:=(IR(3))AND(IR(2))AND(NOT IR(1))AND (NOT IR(0));
	XORR:=(IR(3))AND(NOT IR(2))AND(IR(1))AND (IR(0));
	OUTT:=(IR(3))AND(NOT IR(2))AND(IR(1))AND (NOT IR(0));
	IF W1='1' THEN
		IF FLAG='1' THEN
			--((W2 AND NOT FLAG)OR(W1 AND FLAG))
			HI:='1';
			--((W1 AND NOT FLAG)OR(W2 AND FLAG))
			HII:='0';
		ELSE
			HI:='0';
			HII:='1';
		END IF;
	ELSE 
		IF FLAG='1' THEN
			HI:='0';
			HII:='1';
		ELSE
			HI:='1';
			HII:='0';
		END IF;
	END IF;
	RM:=(NOT SWC)AND(SWB)AND(NOT SWA);
	WM:=(NOT SWC)AND(NOT SWB)AND(SWA);
	RR:=(NOT SWC)AND(SWB)AND(SWA);
	WR:=(SWC)AND(NOT SWB)AND(NOT SWA);
	FE:=(NOT SWC)AND(NOT SWB)AND(NOT SWA)AND(ST0);
	FEP:=(NOT SWC)AND(NOT SWB)AND(NOT SWA)AND(NOT ST0);
	LIR<=(FE AND ( NOP OR ADD OR SUB OR ANDD OR INC OR(JC AND((NOT C)OR (C AND(HI))))OR (JZ AND((NOT Z)OR (Z AND(HI)))) OR ((JMP)AND(HI)) OR XORR OR OUTT OR ORR OR NOTT OR STP OR ((LD)AND(HI))OR((ST)AND(HI))));
	PCINC<=(FE AND ( NOP OR ADD OR SUB OR ANDD OR INC OR (JC AND((NOT C)OR (C AND(HI)))) OR (JZ AND((NOT Z)OR (Z AND(HI)))) OR ((JMP)AND(HI)) OR XORR OR OUTT OR ORR OR NOTT OR STP OR ((LD)AND(HI))OR((ST)AND(HI)))) ;
	M<=(FE AND (ANDD OR(LD AND HII )OR (ST AND (HI))OR (JMP AND HII) OR OUTT OR XORR OR ORR OR NOTT ));
	S(3)<=(FE AND (ADD OR ANDD OR LD OR ST OR JMP OR OUTT OR ORR ));
	S(2)<=(FE AND (SUB OR (ST AND HII) OR JMP OR XORR OR ORR OR NOTT));
	S(1)<=(FE AND(SUB OR ANDD OR LD OR ST OR JMP OR OUTT OR XORR OR ORR));
	S(0)<=(FE AND(ADD OR ANDD OR (ST AND HII) OR JMP OR NOTT));
	CIN<=(FE AND(ADD));
	--CIN<=CFLAG;
	--LPC<=(FE AND (JMP ));
	MBUS<=(FE AND ((LD)AND(HI)))OR RM;
	MEMW<=(FE AND((ST)AND(HI)))OR(WM AND ST0);
	STOP<=(FE AND STP) OR RM OR WM OR RR OR WR OR FEP;
--FLAG<=(FE AND( (W1 AND(NOP OR ADD OR SUB OR ANDD OR INC OR JC OR JZ OR JMP OR OUTT OR XORR OR ORR OR NOTT OR STP))OR(FLAG AND (LD OR ST))));
	M<=(FE AND (ANDD OR(LD AND HII )OR (ST AND (HI))OR JMP OR OUTT OR XORR OR ORR OR NOTT ));
	LDC<=(FE AND (ADD OR SUB OR INC));
	--LDC<='1';
	LDZ<=(FE AND (ADD OR SUB OR ANDD OR INC OR XORR OR ORR OR NOTT));
	DRW<=(FE AND (ADD OR SUB OR ANDD OR INC OR (LD AND HI )OR  XORR OR ORR OR NOTT ))OR WR;
	ABUS<=(FE AND (ADD OR SUB OR ANDD OR INC OR (LD AND HII )OR  XORR OR ORR OR NOTT OR ST OR (JMP AND HII)OR OUTT ));
	LAR<=(FE AND ((LD AND HII )OR(ST AND HII )   ))OR (WM and (not ST0)) OR (RM AND (NOT ST0));
	PCADD<=(FE AND ((JC AND C) OR (JZ AND Z) ));
	SHORT<=(FE AND NOP)OR WM OR RM OR FEP;
	SELCTL<=(WM)OR(RM)OR WR or RR;
	ARINC<=(WM AND ST0)OR (RM AND ST0);
	
	--JZ AND((NOT Z)OR (Z AND(((W2 AND NOT FLAG)OR(W1 AND FLAG))))) 
	SBUS<=(WM)OR(RM AND NOT ST0)OR WR OR (FEP );
	SST0<=(WM AND NOT ST0)OR (RM AND NOT ST0) OR (WR AND NOT ST0 AND W2) OR FEP;
	SEL3<=( (WR AND ST0) OR (RR AND W2) );
	SEL2<=( (WR AND (NOT ST0) AND W2) OR (WR AND ST0 AND W2) );
	SEL1<=( (WR AND (NOT ST0) AND W1) OR (WR AND ST0 AND W2) OR (RR AND W2) );
	SEL0<=( (WR AND (NOT ST0) AND W1) OR (WR AND ST0 AND W1) OR RR );
	LPC<=FEP OR (FE AND (JMP AND HII));
	CFLAG<=FE and(ADD OR SUB OR ANDD OR INC OR (JC AND NOT C) OR (JZ AND NOT Z) OR JMP OR OUTT OR XORR OR ORR OR NOTT OR STP);
	END PROCESS;
end struct;