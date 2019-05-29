// ej 1.1
let qf = {
    esFinal: true, 
    transiciones: {}, 
    acepta: function(s){return s.length == 0;}
};

// ej 1.2
let q3 = {
    esFinal: true,
    transiciones: {}
};

let q2 = {
    esFinal: true,
    transiciones: {'c': q3}
};

let q1 = {
    esFinal: false,
    transiciones: {'a': this, 'b': q2, 'c': q3}
};

// ej 2
String.prototype.head = function(){return this[0]};
String.prototype.tail = function(){return this.substr(1,this.length)};

// ej 3.1
function Estado(esFinal, transiciones) {
    this.esFinal = esFinal;
    this.transiciones = transiciones;
};

Estado.prototype.acepta = function(s){
    if(s.length == 0){
        return this.esFinal;
    } else {
        sig = this.transiciones[s.head()]
        return sig != undefined && sig.acepta(s.tail())
    }
};

// ej 3.2
[q1,q2,q3].forEach(function(q){
    Object.setPrototypeOf(q, Estado.prototype);
});

// ej4
Estado.prototype.nuevaTransicion = function(etiqueta, destino){
    this.transiciones[etiqueta] = destino;
};

// ej5 
algunoAcepta = function(s, qs){
    if (Array.isArray(qs)) {
        return qs.some(function(q) {
            return q.acepta(s);
        });
    } else {
        return qs.acepta(s)
    }
};


// ej 6
Estado.prototype.nuevaTransicionND = function(etiqueta, destino) {
    if (this.transiciones[etiqueta] == undefined) {
        this.nuevaTransicion(etiqueta, destino);
    } else {
        if (Array.isArray(this.transiciones[etiqueta])){
            if (!this.transiciones[etiqueta].includes(destino)) {
                this.transiciones[etiqueta].push(destino);
            }
        } else {
            if (this.transiciones[etiqueta] != destino) {
                this.transiciones[etiqueta] = [this.transiciones[etiqueta], destino];
                this.acepta = function(s) {
                    if(s.length == 0){
                        return this.esFinal;
                    } else {
                        sig = this.transiciones[s.head()]
                        if (Array.isArray(sig)) {
                            return sig.some(function(e) {return e.acepta(s.tail())})
                        } else {
                            return sig != undefined && sig.acepta(s.tail())
                        }
                    }
                }
            }
        }
    }
};

// ej 7
Estado.prototype.destinos = function(){
    dest = [];
    Object.values(this.transiciones).forEach(function(estados) {
        if(Array.isArray(estados)){
            dest = dest.concat(estados);
        }else{
            dest.push(estados);
        }
    });
    return dest;
}

esDetSinRepetir = function(q, visitados) {
    esDet = true;
    Object.keys(q.transiciones).forEach(function(etiqueta){
        destino = q.transiciones[etiqueta];

        if(Array.isArray(destino)){
            esDet = false;
        }else{
            if (!visitados.includes(destino)){
                visitados.push(destino);
                esDet = esDet && esDetSinRepetir(destino, visitados);
            }
        }
    });

    return esDet;
}

esDeterministico = function(q) {
    return esDetSinRepetir(q, [q]);
}

function calcularResultado(){
	//Editen esta función para que devuelva lo que quieran ver. Pueden escribir acá sus tests.
    e3 = new Estado(true, {});
    e2 = new Estado(false, {'b': e3});
    e1 = new Estado(false, {'a': e2});

    console.log("e1.acepta('') " + e1.acepta(''));
    console.log("e1.acepta('ab') " + e1.acepta('ab'));
    console.log("e3.acepta('') " + e3.acepta(''));
    return 'mira la consola wacho';
}
