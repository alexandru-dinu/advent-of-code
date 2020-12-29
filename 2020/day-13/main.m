xs = strsplit(fileread(argv(){1}), '\n');
earliest = str2num(xs{1});
busids = strsplit(xs{2}, ',');

rs = []; % remainders
ms = []; % moduli (bus ids)
for i = 1:length(busids)
    if busids{i} ~= 'x'
        m = str2num(busids{i});
        ms = [ms, m];
        rs = [rs, mod(-(i-1), m)];
    end
end


[tmin, imin] = min(arrayfun(@(b) b * ceil(earliest / b), ms));
printf("Part 1: %d\n", ms(imin) * (tmin - earliest));


% TODO: smarter
function x = modinv(a, m)
    for x = 1:m
        if mod(a * x, m) == 1
            return;
        end
    end
endfunction


function t = crt(rs, ms)
    M = prod(ms);

    t = 0;
    for i = 1:length(rs)
        q = idivide(M, int64(ms(i)), 'fix');
        t += rs(i) * q * modinv(q, ms(i));
    end

    t = mod(t, M);
endfunction


xs = (0 <= rs < ms);
assert(sum(xs == 1) == length(xs), "Remainder must be in [0, modulus)!");

xs = nchoosek(ms, 2);
xs = gcd(xs(:, 1), xs(:, 2));
assert(sum(xs == 1) == length(xs), "Moduli must be pairwise coprime!");

printf("Part 2: %d\n", crt(rs, ms));