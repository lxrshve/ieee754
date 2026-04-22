#include<iostream>
#include<string>
#include<iomanip>
template< uint16_t len_exp, uint32_t len_mant, bool HasInf, uint16_t round, bool HasNan> 
struct Float {
    uint32_t static create_number(uint32_t number) {
        if((len_exp + len_mant + 1) >= 32) {
            return number;
        }
        uint32_t mask = (uint32_t(1) << (len_exp + len_mant + 1)) - 1; 
        return number & mask; 
    }
    uint32_t static get_sign(uint32_t number)
    {
        return ((number >> (len_exp + len_mant)) & 0x1);
    }
    uint32_t static get_mant(uint32_t number)
    {
        return (number & (((uint32_t)1 << (len_mant)) - 1));
    }
    int32_t static get_exp(uint32_t number)
    {
        return ((number >> len_mant) & (((uint32_t)1 << len_exp) - 1));
    }

    bool static Nan(uint32_t number)
    {
        if (!HasNan) {
            return false;
        }
        if (len_exp == 4 && len_mant == 3 && !HasInf) {
            return (get_exp(number) == 15 && get_mant(number) == 7);
        }
        if (get_exp(number) == ((1 << len_exp) - 1) && get_mant(number) != 0 && HasNan) {
            return true;
        }
        return false;
    }
    bool static Inf(uint32_t number)
    {
        if(!HasInf)
        {
            return false;
        }

        if (get_exp(number) == ((1 << len_exp) - 1) && get_mant(number) == 0) {
            return true;
        }
        return false;
    }
    bool static DeNormal(uint32_t number)
    {
        if(get_exp(number) == 0 && get_mant(number) != 0)
        {
            return true;
        }
        return false;
    }
    bool static Zero(uint32_t number)
    {
        if(get_exp(number) == 0 && get_mant(number) == 0)
        {
            return true;
        }
        return false;
    }

    void static Print(uint32_t number)
    {
        number = create_number(number);
        bool is_nan = Nan(number);
        bool is_inf = Inf(number);
        bool is_denormal = DeNormal(number);
        bool is_zero = Zero(number);
        uint32_t count = (len_mant + 3) / 4;
        uint32_t count2 = (len_exp + len_mant + 1) / 4;
        if (is_nan) {
            std::cout << "nan" << ' ' << "0x" << std::uppercase << std::hex << std::setw(count2) << std::setfill('0') << number << std::endl;
            return;
        }
        if(is_inf) {
            if(get_sign(number) == 0) {
                std::cout << "inf" << ' ' << "0x" << std::uppercase << std::hex << std::setw(count2) << number << std::endl;
                return;
            } else {
                std::cout << "-inf" << ' ' << "0x" << std::uppercase << std::hex << std::setw(count2) << number << std::endl;
                return;
            }
        }
        if (is_zero) {
            if (get_sign(number) == 1) {std::cout << '-';}
            std::cout << "0x0." << std::nouppercase << std::hex << std::setw(count) 
            << std::setfill('0') << get_mant(number) << "p+0" << " 0x" << std::uppercase 
            << std::hex << std::setw(count2) << number << std::endl;
            return;
        }
        uint32_t mant;
        int64_t exp;
        uint16_t sign = get_sign(number);
        if (sign == 1) {
            std::cout << '-';
        } 
        if(is_denormal) {
            mant = get_mant(number);
            exp = 1 - ((1 << (len_exp - 1)) - 1);
            while ((mant & (uint32_t(1) << len_mant)) == 0) {
                mant = mant << 1;
                exp -= 1;
            }
            mant &= (uint32_t(1) << len_mant) - 1;
        } else {
            mant = get_mant(number);
            exp = (int64_t)get_exp(number) - ((1 << (len_exp - 1)) - 1);    
        }
        mant = mant << (count * 4 - len_mant);
        std::cout << "0x1." << std::nouppercase <<std::hex 
        << std::setw(count) << std::setfill('0') << mant 
        << std::dec << 'p' << (exp >= 0 ? "+" : "") << exp << " 0x" << 
        std::uppercase << std::hex << std::setw(count2) << std::setfill('0') << number << std::endl;
    }
    static uint32_t RoundMult(uint32_t number, uint32_t ost, uint16_t sign, int32_t* exp) {
        if (round == 1) {
            uint32_t half = uint32_t(1) << (len_mant - 1);
            if (ost == half) {
                number += number % 2;
            } else if (ost > half) {
                number += 1;
            }
        } else if (round == 2) {
            if (sign == 0 && ost != 0) {
                number += 1;
            }
        } else if (round == 3) {
            if (sign == 1 && ost != 0) {
                number += 1;
            }
        }
        if (number & (uint32_t(1) << (len_mant + 1))) {
            number >>= 1;
            *exp += 1;
        }
        return number;
    }

    static void Normalize(uint32_t* number, int32_t* exp, bool denormal) {
        if (denormal) {
            *exp = 1;
            while ((*number & (uint32_t(1) << len_mant)) == 0) {
                *number = *number << 1;
                *exp -= 1;
            }
        } else {
            *number += (uint32_t(1) << len_mant);
        }
       
    }
    static uint32_t Assemble(uint32_t res, int32_t* exp, uint64_t ost, uint16_t sign, uint64_t stick = 0) {
        while((res >= (uint32_t(1) << (len_mant + 1))) ) {
            if(ost != 0) {
                stick = 1;
            }
            ost = (ost >> 1) + ((res & 1) << (len_mant - 1));
            res >>= 1;
            *exp += 1;
            
        }
        while(*exp > 1 && (res & (uint32_t(1) << (len_mant))) == 0) {
            res = (res << 1) | (ost >> (len_mant - 1));
            ost = (ost << 1) & ((uint32_t(1) << len_mant) - 1);
            *exp -= 1;
      
        }        
        while(*exp < 1 && res != 0) {
            if(ost != 0) {
                stick = 1;
            }
            ost = (ost >> 1) + ((res & 1) << (len_mant - 1));
            res >>= 1;
            *exp += 1;
            
        }
        if (*exp < 1 && res == 0) {
            *exp = 0; 
            ost = 1; 

        }
        if(*exp == 1) {
             if ((res & (uint32_t(1) << len_mant))== 0) {
                *exp = 0;
             }
        }
        if (stick != 0) {
            ost |= 1;
        }
        res = RoundMult(res, ost, sign, exp);
        while (res & (uint32_t(1) << (len_mant + 1))) {
            res >>= 1;
            *exp += 1;
        }
        if (*exp == 0 && (res & (uint32_t(1) << len_mant))) {
            *exp = 1;
        }
        if (*exp == 0 && res == 0) {
            return (uint32_t)sign << (len_exp + len_mant);
        }
        if (*exp >= ((uint32_t(1) << len_exp) - 1)) {
            if ((round == 0 || (round == 2 && sign == 1) || (round == 3 && sign == 0))) {
                *exp = ((uint32_t(1) << len_exp) - 2);
                res = (uint32_t(1) << len_mant) - 1;
            
            } else {
                *exp = (uint32_t(1) << len_exp) - 1;
                res = 0;
            }
        }
        res &= (uint32_t(1) << len_mant) - 1;
        res += (uint32_t(*exp) << len_mant);
        res += (uint32_t)sign << (len_exp + len_mant);
        return res;
    }
    
    static uint32_t CheckAdd(uint32_t number1, uint32_t number2, bool* flag) {
        uint32_t sign1 = get_sign(number1);
        uint32_t sign2 = get_sign(number2);
        bool denormal1 = DeNormal(number1);
        bool denormal2 = DeNormal(number2);
        bool inf1 = Inf(number1);
        bool inf2 = Inf(number2);
        bool nan1 = Nan(number1);
        bool nan2 = Nan(number2);
        bool zero1 = Zero(number1);
        bool zero2 = Zero(number2);
        if (nan1) {
            *flag = true;
            return number1 | (uint32_t(1) << (len_mant - 1));
        } else if (nan2) {
            *flag = true;
            return number2 | (uint32_t(1) << (len_mant - 1));
        }
        if (inf1 && inf2 && sign1 != sign2) {
            *flag = true;
            return kNaN;
        }
        if(inf1 && inf2 && sign1 == sign2) {
            *flag = true;
            return number1;
        }
        if (inf2) {
            *flag = true;
            return number2;
        }
        if (inf1) {
            *flag = true;
            return number1;
        }
        if (zero1 && zero2) {
            *flag = true;
            if (sign1 == 1 && sign2 == 1) {
                return (uint32_t(1) << (len_exp + len_mant));
            } else if (sign1 != sign2 && round == 3) {
                return (uint32_t(1) << (len_exp + len_mant));
            }
            return 0;
        }
        if (Zero(number1)) {
            *flag = true;
            return number2;
        }
        if (Zero(number2)) {
            *flag = true;
            return number1;
        }
        return 0;
    }
    static const uint32_t kNaN = (((uint32_t(1) << (len_exp + len_mant))) + (((uint32_t(1) << (len_exp)) - 1) << len_mant) + (uint32_t(1) << (len_mant - 1)));
    static uint32_t Add(uint32_t number1, uint32_t number2)
    {
        number1 = create_number(number1);
        number2 = create_number(number2);
        uint64_t stick = 0;
        uint32_t mant1 = get_mant(number1);
        int32_t exp1 = get_exp(number1);
        uint32_t sign1 = get_sign(number1);
        uint32_t mant2 = get_mant(number2);
        int32_t exp2 = get_exp(number2);
        uint32_t sign2 = get_sign(number2);
        bool denormal1 = DeNormal(number1);
        bool denormal2 = DeNormal(number2);
        bool flag = false;
        uint32_t r = CheckAdd(number1, number2, &flag);
        if(flag) {
            return r;
        }
        Normalize(&mant1, &exp1, denormal1);
        Normalize(&mant2, &exp2, denormal2);
        uint64_t m1 = uint64_t(mant1) << len_mant;
        uint64_t m2 = uint64_t(mant2) << len_mant;

        int32_t exp;
        uint32_t sub_exp;
        uint32_t ost1;
        if (exp1 >= exp2) {
            sub_exp = exp1 - exp2;
            exp = exp1;
            if(sub_exp > 2*(len_mant + 1)) {
                m2 = 0;
                stick = 1;
            } else if (sub_exp > 0) {
                uint64_t last = m2 & ((uint64_t(1) << sub_exp) - 1);
                m2 = m2 >> sub_exp;
                if(last) {
                    stick = 1;
                }
            }
        } else {
            sub_exp = exp2 - exp1;
            exp = exp2;
            if(sub_exp > 2*(len_mant + 1)) {
                m1 = 0;
                stick = 1;
            } else if (sub_exp > 0) {
                uint64_t last = m1 & ((uint64_t(1) << sub_exp) - 1);
                m1 = m1 >> sub_exp;
                if(last) {
                    stick = 1;
                }
            }
        }
        uint16_t sign;
        uint64_t result;
        uint64_t m1_1 = (m1 << 1) | ((exp2 > exp1) ? stick : 0);
        uint64_t m2_2 = (m2 << 1) | ((exp1 >= exp2) ? stick : 0);
        
        if (sign1 == sign2) {
            sign = sign1;
            result = m1_1 + m2_2;
        } else {
            if (m1_1 >= m2_2) {
                sign = sign1;
                result = m1_1 - m2_2;
            } else {
                sign = sign2;
                result = m2_2 - m1_1;   
            }
        }
        stick = result & 1;
        result = result >> 1;
        if (result == 0) {
            uint16_t sign = (round == 3) ? 1 : 0;
            return sign << (len_exp + len_mant);
        }
        uint64_t res = result >> len_mant;
        uint32_t ost = result & ((uint32_t(1) << len_mant) - 1);
        return Assemble(res, &exp, ost, sign, stick);
    }
    static uint32_t Sub(uint32_t number1, uint32_t number2)
    {
        if (Nan(number1)) return number1 | (uint32_t(1) << (len_mant - 1));
        if (Nan(number2)) return number2 | (uint32_t(1) << (len_mant - 1));
        uint32_t sign2 = get_sign(number2);
        if (sign2 == 0) {
            number2 += (uint32_t(1) << (len_exp + len_mant));
        } else {
            number2 -= (uint32_t(1) << (len_exp + len_mant));
        }
        return Add(number1, number2);
    }

    
    static uint32_t CheckMult(uint32_t number1, uint32_t number2, bool* flag) {
        uint32_t sign1 = get_sign(number1);
        uint32_t sign2 = get_sign(number2);
        bool denormal1 = DeNormal(number1);
        bool denormal2 = DeNormal(number2);
        bool inf1 = Inf(number1);
        bool inf2 = Inf(number2);
        bool nan1 = Nan(number1);
        bool nan2 = Nan(number2);
        bool zero1 = Zero(number1);
        bool zero2 = Zero(number2);
        if(nan1) {
            *flag = true;
            return number1 | (uint32_t(1) << (len_mant - 1));
        } else if (nan2) {
            *flag = true;
            return number2 | (uint32_t(1) << (len_mant - 1));
        }
        if ((inf1 && zero2) || (inf2 && zero1)) {
            *flag = true;
            return kNaN;    
        }
        if (inf1 || inf2) {
            uint32_t sign = (sign1 == sign2) ? 0 : 1;
            *flag = true;
            return (sign << (len_exp + len_mant)) + (((uint32_t(1) << len_exp) - 1) << len_mant);
        }
        if (zero1 || zero2) {
            
            uint32_t sign = (sign1 == sign2) ? 0 : 1;
            *flag = true;
            return sign << (len_exp + len_mant);
        }
        return 0;
    }
    uint32_t static Mult(uint32_t number1, uint32_t number2) {
        number1 = create_number(number1);
        number2 = create_number(number2);
        bool flag = false;
        uint32_t r = CheckMult(number1, number2, &flag);
        if(flag) {
            return r;
        }

        uint32_t mant1 = get_mant(number1);
        int32_t exp1 = get_exp(number1);
        uint32_t sign1 = get_sign(number1);
        uint32_t mant2 = get_mant(number2);
        int32_t exp2 = get_exp(number2);
        uint32_t sign2 = get_sign(number2);
        bool denormal1 = DeNormal(number1);
        bool denormal2 = DeNormal(number2);
        Normalize(&mant1, &exp1, denormal1);
        Normalize(&mant2, &exp2, denormal2);
        int32_t exp = exp1 + exp2 - ((1 << (len_exp - 1)) - 1);

        uint16_t sign = (sign1 == sign2) ? 0 : 1;
        uint64_t result;
        result = uint64_t(mant1) * mant2;
        uint32_t ost = result & ((uint32_t(1) << (len_mant)) - 1);
        uint32_t res = result >> (len_mant);
        res = Assemble(res, &exp, ost, sign);
        return res;
        
    }
    static uint32_t CheckDiv(uint32_t number1, uint32_t number2, bool* flag) {
        uint32_t sign1 = get_sign(number1);
        uint32_t sign2 = get_sign(number2);
        bool denormal1 = DeNormal(number1);
        bool denormal2 = DeNormal(number2);
        bool inf1 = Inf(number1);
        bool inf2 = Inf(number2);
        bool nan1 = Nan(number1);
        bool nan2 = Nan(number2);
        bool zero1 = Zero(number1);
        bool zero2 = Zero(number2);
        uint16_t sign = (sign1 == sign2) ? 0 : 1;
        if(nan1) {
            *flag = true;
            return number1 | (uint32_t(1) << (len_mant - 1));
        } else if (nan2) {
            *flag = true;
            return number2 | (uint32_t(1) << (len_mant - 1));
        }
        if (inf1 && inf2) {
            *flag = true;
            return kNaN;        
        }
        if (inf1) {
            *flag = true;
            return (sign << (len_exp + len_mant)) + (((uint32_t(1) << len_exp) - 1) << len_mant);
        }
        if (inf2) {
            *flag = true;
            return (uint32_t(sign) << (len_exp + len_mant));
        }
        if(zero2) {
            *flag = true;
            if (zero1) {
                return kNaN;
            }
            return (sign << (len_exp + len_mant)) + (((uint32_t(1) << len_exp) - 1) << len_mant);
        }
        if (zero1) {
            *flag = true;
            return (uint32_t)sign << (len_exp + len_mant);
        }
        return 0;
    }

    static uint32_t Div(uint32_t number1, uint32_t number2) {
        number1 = create_number(number1);
        number2 = create_number(number2);
        uint64_t stick = 0;
        uint32_t mant1 = get_mant(number1);
        int32_t exp1 = get_exp(number1);
        uint32_t sign1 = get_sign(number1);
        uint32_t mant2 = get_mant(number2);
        int32_t exp2 = get_exp(number2);
        uint32_t sign2 = get_sign(number2);
        bool denormal1 = DeNormal(number1);
        bool denormal2 = DeNormal(number2);
        uint16_t sign = (sign1 == sign2) ? 0 : 1;
        bool flag = false;
        uint32_t r = CheckDiv(number1, number2, &flag);
        if(flag) {
            return r;
        }
        Normalize(&mant1, &exp1, denormal1);
        Normalize(&mant2, &exp2, denormal2);
        int32_t exp = exp1 - exp2 + ((uint32_t(1) << (len_exp - 1)) - 1) - 4;
        uint64_t mant_1 = (uint64_t)(mant1) << (len_mant + 4);
        uint64_t result = mant_1 / mant2;
        
        uint64_t ost = ((mant_1 % mant2) << len_mant) / mant2; 
        return Assemble(uint32_t(result), &exp, ost, sign, stick);
    }
    static uint32_t AddFma(uint64_t mant1, uint32_t number2, int32_t exp1, int32_t exp2, uint16_t sign1, uint16_t sign2) {
        uint64_t stick = 0;
        int32_t exp;
        uint32_t sub_exp;
        uint32_t ost1;
        uint32_t mant2 = get_mant(number2);
        uint64_t m1 = mant1;
        uint64_t m2;
        if (Zero(number2)) {
            m2 = 0;
            exp2 = exp1; 
        } else {
            Normalize(&mant2, &exp2, DeNormal(number2));
            m2 = (uint64_t)(mant2) << len_mant;
        }
        if (exp1 >= exp2) {
            sub_exp = exp1 - exp2;
            exp = exp1;
            if(sub_exp >= 64) {
                m2 = 0;
                stick = 1;
            } else if (sub_exp > 0) {
                uint64_t last = m2 & ((uint64_t(1) << sub_exp) - 1);
                m2 = m2 >> sub_exp;
                if(last) {
                    stick = 1;
                }
            }
        } else {
            sub_exp = exp2 - exp1;
            exp = exp2;
            if(sub_exp >= 64) {
                m1 = 0;
                stick = 1;
            } else if (sub_exp > 0) {
                uint64_t last = m1 & ((uint64_t(1) << sub_exp) - 1);
                m1 = m1 >> sub_exp;
                if(last) {
                    stick = 1;
                }
            }
        }
        uint16_t sign;
        uint64_t result;
        uint64_t m1_1 = (m1 << 1) | ((exp2 > exp1) ? stick : 0);
        uint64_t m2_2 = (m2 << 1) | ((exp1 >= exp2) ? stick : 0);
        
        if (sign1 == sign2) {
            sign = sign1;
            result = m1_1 + m2_2;
        } else {
            if (m1_1 >= m2_2) {
                sign = sign1;
                result = m1_1 - m2_2;
            } else {
                sign = sign2;
                result = m2_2 - m1_1;   
            }
        }
        stick = result & 1;
        result = result >> 1;
        
        if (result == 0) {
            uint16_t sign = (round == 3) ? 1 : 0;
            return sign << (len_exp + len_mant);
        }
        uint64_t res = result >> len_mant;
        uint32_t ost = result & ((uint32_t(1) << len_mant) - 1);
        
        return Assemble(res, &exp, ost, sign, stick);
    }
    static uint32_t FMA(uint32_t number1, uint32_t number2, uint32_t number3) {
        number1 = create_number(number1);
        number2 = create_number(number2);
        number3 = create_number(number3);
        uint32_t mant1 = get_mant(number1);
        uint32_t mant2 = get_mant(number2);
        uint32_t mant3 = get_mant(number3);
        uint16_t sign1 = get_sign(number1);
        uint16_t sign2 = get_sign(number2);
        uint16_t sign3 = get_sign(number3);
        int32_t exp1 = get_exp(number1);
        int32_t exp2 = get_exp(number2);
        int32_t exp3 = get_exp(number3);
        bool denormal1 = DeNormal(number1);
        bool denormal2 = DeNormal(number2);
        bool denormal3 = DeNormal(number3);
        Normalize(&mant1, &exp1, denormal1);
        Normalize(&mant2, &exp2, denormal2);

        bool flag = false;
        CheckMult(number1, number2, &flag);
        uint64_t m1m2;
        int32_t exp_m1m2;
        if(flag) {
            m1m2 = CheckMult(number1, number2, &flag);
            exp_m1m2 = 0;
        } else {
            m1m2 = (uint64_t)mant1 * mant2;
            exp_m1m2 = exp1 + exp2 - ((1 << (len_exp - 1)) - 1);
        }
        if(flag) {
            return Add(m1m2, number3);
        }

        if (Nan(number3)) {
            return number3 | (uint32_t(1) << (len_mant - 1));
        }
        if (Inf(number3)) {
            return number3;
        }
        return AddFma(m1m2, number3, exp_m1m2, exp3, (sign1 == sign2) ? 0 : 1, sign3);

    }
    static uint32_t Mad(uint32_t n1, uint32_t n2, uint32_t n3) {
        return Add(Mult(n1, n2), n3);
    }
};