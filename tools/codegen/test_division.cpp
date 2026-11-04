#include <bit>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <limits>
#include <random>
using U = unsigned long long;
using W = unsigned __int128;
#define __device__
#define __forceinline__ inline
static U __umul64hi(U a,U b) { return U((W(a)*b)>>64); }
static unsigned __umulhi(unsigned a,unsigned b) { return unsigned((uint64_t(a)*b)>>32); }
#include "CertifiedDivision.cuh"
#define require(p) do { if(!(p)) { std::fprintf(stderr,"failure line %d\n",__LINE__); std::abort(); } } while(0)
static U jam(U a,unsigned d) { return d>=64 ? a!=0 : d==0 ? a : (a>>d)|((a&((U(1)<<d)-1))!=0); }
static U pack(bool sign,int e,U q) {
    if(e<=0) { q=jam(q,unsigned(1-e)); e=0; }
    U sig=q>>3, r=q&7;
    sig+=(r>4 || (r==4 && (sig&1)));
    if(e==0) { if(sig>=(U(1)<<52)) e=1; }
    else if(e==1 && sig<(U(1)<<52)) e=0;
    else if(sig>=(U(1)<<53)) { sig>>=1; ++e; }
    if(e>=2047) return (U(sign)<<63)|0x7ff0000000000000ull;
    return (U(sign)<<63)|(U(e)<<52)|(sig&0x000fffffffffffffull);
}
static bool grid(U u,U v,U z) {
    const W numerator=W(u)<<63;
    const U base=z&~511ull;
    for(int j=-1;j<=1;++j) {
        const U k=base+U(j*512ll);
        const W rhs=W(k)*v;
        const int exact=(numerator>rhs)-(numerator<rhs);
        const int proxy=(z>k)-(z<k);
        if(exact!=proxy) return false;
    }
    return true;
}
static unsigned reciprocal(unsigned b,unsigned k0,unsigned k1) {
    const unsigned eps=(b>>11)&65535;
    const unsigned r0=k0-((k1*eps)>>20);
    const unsigned sigma=~unsigned((uint64_t(r0)*b)>>7);
    unsigned r=(r0<<16)+unsigned((uint64_t(r0)*sigma)>>24);
    const unsigned square=unsigned((uint64_t(sigma)*sigma)>>32);
    return r+unsigned((uint64_t(r)*square)>>48);
}
int main() {
    require(grid(1ull<<62,1ull<<63,1ull<<62));
    require(!grid(1ull<<62,1ull<<63,(1ull<<62)+1));
    require(!grid(1ull<<62,1ull<<63,(1ull<<62)-1));
    U z=0; bool accepted=false,boundary=false,repaired=false;
    soft_certified_division_leaf(1ull<<62,1ull<<63,0,0,&z,&accepted,&boundary,&repaired);
    require(!accepted);
    std::mt19937_64 rng(0xC371F1ED);
    uint64_t certificateRejects=0, certificateAccepts=0;
    for(unsigned i=0;i<1000000;++i) {
        const U b=0x80000000ull|unsigned(rng()), v=b<<32;
        const unsigned index=unsigned(b>>27)&15;
        unsigned k0=soft_reciprocal_k0[index],k1=soft_reciprocal_k1[index];
        if(i%3==0) { k0=unsigned(rng());k1=unsigned(rng()); }
        if(i%3==1) { k0^=1u<<unsigned(rng()%32);k1^=1u<<unsigned(rng()%32); }
        U r=reciprocal(unsigned(b),k0,k1);
        const W C=W(1)<<63,raw=W(b)*r;
        const bool repair=raw<C && C-raw>=2*b;
        if(repair)++r;
        const bool exact=raw<C && W(b)*r<C && C-W(b)*r<2*b;
        soft_certified_division_leaf(v>>1,v,k0,k1,&z,&accepted,&boundary,&repaired);
        require(accepted==exact && repaired==repair);
        if(accepted)require(grid(v>>1,v,z));
        certificateRejects+=!accepted;
        certificateAccepts+=accepted;
    }
    require(certificateRejects>0 && certificateAccepts>0);
    std::printf("1000000 selected/arbitrary/corrupted coefficient pairs: exact uint128 certificate agreement; rejected=%llu accepted=%llu\n",(unsigned long long)certificateRejects,(unsigned long long)certificateAccepts);

    uint64_t refinements=0, repairs=0;
    for(unsigned i=0;i<4000000;++i) {
        const U ea=1+rng()%2046, eb=1+rng()%2046;
        U a=(ea<<52)|(rng()&0x000fffffffffffffull);
        U b=(eb<<52)|(rng()&0x000fffffffffffffull);
        if(i%7==0) b=(eb<<52);
        if(i%11==0) b=(eb<<52)|0xfffffffffffffull;
        if(i%13==0) a=b+U(int(i%3)-1);
        if (((a>>52)&2047)==0) a=1ull<<52;
        if (((a>>52)&2047)==2047) a=0x7fefffffffffffffull;
        a|=(rng()&1)<<63; b|=(rng()&1)<<63;
        const U A=(a&0xfffffffffffffull)|(1ull<<52),B=(b&0xfffffffffffffull)|(1ull<<52);
        const bool below=A<B;
        const U v=B<<11,u=below?A<<11:A<<10;
        const unsigned index=unsigned(v>>59)&15;
        soft_certified_division_leaf(u,v,soft_reciprocal_k0[index],soft_reciprocal_k1[index],&z,&accepted,&boundary,&repaired);
        require(accepted); require(grid(u,v,z)); refinements+=boundary;
        repairs+=repaired;
        const U bits=pack(((a^b)>>63)!=0,int((a>>52)&2047)-int((b>>52)&2047)+1023-int(below),jam(z,7));
        const U ref=std::bit_cast<U>(std::bit_cast<double>(a)/std::bit_cast<double>(b));
        if(bits!=ref) std::fprintf(stderr,"a=%llx b=%llx bits=%llx ref=%llx\n",a,b,bits,ref);
        require(bits==ref);
    }
    require(repairs>0);
    std::printf("generated leaf: 4000000 signed normal pairs, %llu table repairs, %llu refinements; native binary64 and exact uint128 grid PASS; wrong-grid controls rejected\n",(unsigned long long)repairs,(unsigned long long)refinements);
}
