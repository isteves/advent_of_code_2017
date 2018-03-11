#test
elements = c(0, 1, 2, 3, 4)
lengths = c(3, 4, 1, 5)

#puzzle input
elements = 0:255
lengths = c(183,0,31,146,254,240,223,150,2,206,161,1,255,232,199,88)

skip_size = 0
curr_pos = 1

for(i in lengths){
    i = i - 1
    if(i != 0){
        curr_pos <- curr_pos %% length(elements) #give a value within range
        #twist segment
        if(curr_pos + i <= length(elements)){ #if looping not needed
            #extract sublist of loop to twist
            sublist <- elements[curr_pos:(curr_pos + i)]
            #now twist and put it back!
            elements[curr_pos:(curr_pos + i)] <- sublist[length(sublist):1]
            
        } else { #if the segment loops...
            end_pos <- (curr_pos + i) %% length(elements)
            #extract segments from end and beg and combine
            seg1 <- elements[curr_pos:length(elements)]
            seg2 <- elements[1:end_pos]
            sublist <- c(seg1, seg2)
            #twist it!
            elem_twist <- sublist[length(sublist):1]
            #put back into loop
            elements[curr_pos:length(elements)] <- elem_twist[1:length(seg1)]
            elements[1:end_pos] <- elem_twist[(length(seg1) + 1):length(elem_twist)]                
        }
    }
    curr_pos <- curr_pos + i + 1 + skip_size
    skip_size <- skip_size + 1
    # print(elements)
}

print(elements[1] * elements[2])

#part 2 ------------
#various conversions...
#1. turn every character in input (lengths) into ASCII code
#add these to end: 17, 31, 73, 47, 23

#2. run 64 rounds, keep lengths/curr_pos/skip_size for each round

#now reduce ordered numbers ("sparse hash") to dense hash
#XOR blocks of 16 --> get 16 numbers


