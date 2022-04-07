def forward(src, srcIx)
    depth = 1
    while depth > 0
        srcIx += 1
        if (srcIx >= src.length) 
            puts "Out of Bounds"
            exit(1)
        end
        case src[srcIx]
        when '['
            depth += 1
        when ']'
            depth -= 1
        end
    end
    return srcIx
end

def backward(src, srcIx)
    depth = 1
    while depth > 0
        srcIx -= 1
        if (srcIx < 0) 
            puts "Out of Bounds"
            exit(1)
        end
        case src[srcIx]
        when ']'
            depth += 1
        when '['
            depth -= 1
        end
    end
    return srcIx
end

def brainf(src)
    srcIx = 0
    memIx = 0
    mem = Array.new(30000) { 0 }
    while srcIx < src.length
        case src[srcIx]
        when '+'
            mem[memIx] += 1
           if mem[memIx] == 256
               mem[memIx] = 0
           end
        when '-'
            mem[memIx] -= 1
            if mem[memIx] == -1
                mem[memIx] = 255
            end
        when '>'
            memIx = memIx + 1
            if memIx == 30000
                memIx = 0
            end
        when '<'
            memIx = memIx - 1
            if memIx == -1
                memIx = 29999
            end
        when '['
            if mem[memIx] == 0
                srcIx = forward(src, srcIx)
            end
        when ']'
            if mem[memIx] != 0
                srcIx = backward(src, srcIx)
            end
        when '.'
            print mem[memIx].chr
            STDOUT.flush
        when ','
            mem[memIx] = STDIN.getc.ord
        end
        srcIx = srcIx + 1
    end
    puts 
end


file = File.read(ARGV[0])
brainf(file)
