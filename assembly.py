import sys
arrayCount = 0;
class Register:

	inUse = False
	alias  = ''

registerTable = [Register() for i in range(15)]
latest_register = 0

def assignRegister(alias):
	count = 0
	#Register
	for register in registerTable:
		if register.inUse == False and register.alias == '':
			register.inUse = True
			register.alias = alias.rstrip('\n' + ' ')
			#print ("#Assigning Register " + str(count))
			return count
		count+= 1

def findAlias(alias):
	count = 0
	for register in registerTable:
		if register.alias == alias.rstrip('\n' + ' '):
			return count
		count+= 1

def eliminateAlias(alias):
	count = 0
	for register in registerTable:
		if register.alias == alias.rstrip('\n' + ' '):
			register.alias =''
			register.inUse = False
			#print ("#Removing Register " + str(count))
		count += 1	





#For Keeping track of variables
with open('threeAddressCode.txt') as f:
	input = f.readlines()


for line in input:

	if line[0] != '\t':
		MIPSInstruction = ""

		if 'goto' in line:
			instruction = line.split(' ')
			MIPSInstruction += str("j " + str(instruction[:][1]))

		elif 'Branch' in line:
			instruction = line.split(' ')
			MIPSInstruction += str("beq $t" +  str(findAlias(latest_alias)) + ' $0 ' + str(instruction[:][4]))

		#IF Statement
		elif '_L' in line:
			MIPSInstruction += str(line)

		#Additive Expressions
		if  '+' in line:
			instruction = line.split(' ')
			latest_register = assignRegister(instruction[:][0])
			latest_alias = str(instruction[:][0]).strip('\n')
			
			MIPSInstruction += str("add $t" +  str(latest_register) + ', ')
	
			if '_' in instruction[:][2]:

				MIPSInstruction += str("$t" +  str(findAlias(instruction[:][2])) + ', ')


			else:
				offset = instruction[:][2].split('(')
				offset = offset[1][:].split(')')
				latest_register = assignRegister(instruction[:][2])
				print("la $t"+ str(latest_register) + ", " + str(offset[0][:]) + "($sp)" )

				if '^' not in line:
					latest_register = assignRegister(instruction[:][2])
					print("lw $t"+ str(latest_register) + ", " + str(0) + "($t" + str(findAlias(instruction[:][2])) + ")" )
				else:
					arrayCount += 1
				MIPSInstruction += str("$t" +  str(latest_register) + ', ')

			if '_' in instruction[:][4]:
				MIPSInstruction += str("$t" +  str(findAlias(instruction[:][4]))+ ' ')

			else:
				offset = instruction[:][4].split('(')
				offset = offset[1][:].split(')')
				latest_register = assignRegister(instruction[:][4])
				print("la $t"+ str(latest_register) + ", " + str(offset[0][:]) + "($sp)" )
				print("lw $t"+ str(latest_register) + ", " + str(0) + "($t" + str(latest_register) + ")" )
				MIPSInstruction += str("$t" +  str(findAlias(instruction[:][4])) + ', ')


			eliminateAlias(instruction[:][2])
			eliminateAlias(instruction[:][4])




		#Additive Expressions
		#Additive Expressions
		elif  '*' in line:
			instruction = line.split(' ')
			latest_register = assignRegister(instruction[:][0])
			latest_alias = str(instruction[:][0]).strip('\n')
			
			MIPSInstruction += str("mul $t" +  str(latest_register) + ', ')

			if '_' in instruction[:][2]:
				MIPSInstruction += str("$t" +  str(findAlias(instruction[:][2])) + ', ')

			else:
				offset = instruction[:][2].split('(')
				offset = offset[1][:].split(')')
				latest_register = assignRegister(instruction[:][2])
				print("la $t"+ str(latest_register) + ", " + str(offset[0][:]) + "($sp)" )

				if '^' not in line:
					latest_register = assignRegister(instruction[:][2])
					print("lw $t"+ str(latest_register) + ", " + str(0) + "($t" + str(findAlias(instruction[:][2])) + ")" )

				MIPSInstruction += str("$t" +  str(latest_register) + ', ')


			if '_' in instruction[:][4]:
				MIPSInstruction += str("$t" +  str(findAlias(instruction[:][4]))+ ' ')

			else:
				offset = instruction[:][4].split('(')
				offset = offset[1][:].split(')')
				latest_register = assignRegister(instruction[:][4])
				print("la $t"+ str(latest_register) + ", " + str(offset[0][:]) + "($sp)" )
				print("lw $t"+ str(latest_register) + ", " + str(0) + "($t" + str(latest_register) + ")" )
				MIPSInstruction += str("$t" +  str(latest_register) + ' ')
			eliminateAlias(instruction[:][2])
			eliminateAlias(instruction[:][4])

		elif  '-' in line:
			instruction = line.split(' ')
			latest_register = assignRegister(instruction[:][0])
			latest_alias = str(instruction[:][0]).strip('\n')
			
			MIPSInstruction += str("sub $t" +  str(latest_register) + ', ')

			if '_' in instruction[:][2]:
				MIPSInstruction += str("$t" +  str(findAlias(instruction[:][2])) + ', ')

			else:
				offset = instruction[:][2].split('(')
				offset = offset[1][:].split(')')
				latest_register = assignRegister(instruction[:][2])
				print("la $t"+ str(latest_register) + ", " + str(offset[0][:]) + "($sp)" )

				if '^' not in line:
					latest_register = assignRegister(instruction[:][2])
					print("lw $t"+ str(latest_register) + ", " + str(0) + "($t" + str(findAlias(instruction[:][2])) + ")" )
				MIPSInstruction += str("$t" +  str(latest_register) + ', ')


			if '_' in instruction[:][4]:
				MIPSInstruction += str("$t" +  str(findAlias(instruction[:][4]))+ ' ')

			else:
				offset = instruction[:][4].split('(')
				offset = offset[1][:].split(')')
				latest_register = assignRegister(instruction[:][4])
				print("la $t"+ str(latest_register) + ", " + str(offset[0][:]) + "($sp)" )
				print("lw $t"+ str(latest_register) + ", " + str(0) + "($t" + str(latest_register) + ")" )
				MIPSInstruction += str("$t" +  str(latest_register) + ' ')
			eliminateAlias(instruction[:][2])
			eliminateAlias(instruction[:][4])


		elif  '<' in line:
			instruction = line.split(' ')
			latest_register = assignRegister(instruction[:][0])
			latest_alias = str(instruction[:][0]).strip('\n')
			
			MIPSInstruction += str("slt $t" +  str(latest_register) + ', ')
	
			if '_' in instruction[:][2]:

				MIPSInstruction += str("$t" +  str(findAlias(instruction[:][2])) + ', ')


			else:
				offset = instruction[:][2].split('(')
				offset = offset[1][:].split(')')
				latest_register = assignRegister(instruction[:][2])
				print("la $t"+ str(latest_register) + ", " + str(offset[0][:]) + "($sp)" )

				if '^' not in line:
					latest_register = assignRegister(instruction[:][2])
					print("lw $t"+ str(latest_register) + ", " + str(0) + "($t" + str(findAlias(instruction[:][2])) + ")" )
				else:
					arrayCount += 1
				MIPSInstruction += str("$t" +  str(latest_register) + ', ')

			if '_' in instruction[:][4]:
				MIPSInstruction += str("$t" +  str(findAlias(instruction[:][4]))+ ' ')

			else:
				offset = instruction[:][4].split('(')
				offset = offset[1][:].split(')')
				latest_register = assignRegister(instruction[:][4])
				print("la $t"+ str(latest_register) + ", " + str(offset[0][:]) + "($sp)" )
				print("lw $t"+ str(latest_register) + ", " + str(0) + "($t" + str(latest_register) + ")" )
				MIPSInstruction += str("$t" +  str(findAlias(instruction[:][4])) + ', ')


			eliminateAlias(instruction[:][2])
			eliminateAlias(instruction[:][4])

		#Assignment Expressions
		elif  '=' in line:
			instruction = line.split('=')

			#Array
			if '&' in line:
					offset = instruction[:][0].split('(')
					offset = offset[1][:].split(')')
					if arrayCount == 2:
						print("lw $t"+ str(findAlias(latest_alias)) + ", 0($t" + str(findAlias(latest_alias)) + ")" )
					elif arrayCount == 3:
						print "here"
					arrayCount = 0
					print("sw $t"+ str(findAlias(latest_alias)) + ", 0($t" + str(findAlias(offset[0][:])) + ")" )
					eliminateAlias(latest_alias)


		

			# Constant Values
			elif '_' in instruction[:][0]:
				#print (instruction[:][0])
				latest_alias = str(instruction[:][0])
				latest_alias.strip(' ')
				latest_register = assignRegister(str(latest_alias))
			
				if '(' in instruction[:][1]:
					offset = instruction[:][1].split('(')
					offset = offset[1][:].split(')')
					print("sw $t"+ str(findAlias(latest_alias)) + ", " + str(offset[0][:]) + "($sp)" )
					
				else:
					MIPSInstruction += str('li $t' + str(latest_register)  + ',')
					MIPSInstruction += str(instruction[:][1])

			# V = $tX
			else:
				#localVariables[str(instruction[:][0]).rstrip(' ' + '\n')] = instruction[:][1]
				#assignVariable(instruction[:][0], latest_register)

					offset = instruction[:][0].split('(')
					offset = offset[1][:].split(')')
					print("sw $t"+ str(findAlias(latest_alias)) + ", " + str(offset[0][:]) + "($sp)" )
					eliminateAlias(latest_alias)

		if MIPSInstruction is not "":
			print(MIPSInstruction.rstrip('\n'))
	
	




