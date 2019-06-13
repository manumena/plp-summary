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
    transiciones: {'b': q2, 'c': q3}
};
q1.transiciones['a'] = q1

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
aceptaND = function(cadena) {
    if(cadena.length == 0){
        return this.esFinal;
    } else {
        sig = this.transiciones[cadena.head()]
        if (Array.isArray(sig)) {
            return sig.some(function(e) {return e.acepta(cadena.tail())})
        } else {
            return sig != undefined && sig.acepta(cadena.tail())
        }
    }
};

Estado.prototype.nuevaTransicionND = function(etiqueta, destino) {
    if (this.transiciones[etiqueta] == undefined) { //si no tenia transacciones
        this.nuevaTransicion(etiqueta, destino);
    } else {
        if (Array.isArray(this.transiciones[etiqueta])){ //si ya era ND
            if (!this.transiciones[etiqueta].includes(destino)) {
                this.transiciones[etiqueta].push(destino);
            }
        } else {
            if (this.transiciones[etiqueta] != destino) { //si hay que volverlo ND
                this.transiciones[etiqueta] = [this.transiciones[etiqueta], destino];
                this.acepta = aceptaND;
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


// TESTS
assert = function(condition, message) {
    if (!condition) {
        throw message || "Assertion failed";
    }
}

test_ej_3_1_acepta = function() {
    console.log("Corriendo test_ej_3_1_acepta")

    e3 = new Estado(true, {});
    e2 = new Estado(false, {'b': e3});
    e1 = new Estado(false, {'a': e2});

    assert(!e1.acepta(''));
    assert(!e1.acepta('a'));
    assert(e1.acepta('ab'));
    assert(!e2.acepta(''));
    assert(e2.acepta('b'));
    assert(e3.acepta(''));

    console.log("[PASA]")
}

test_ej_3_2_acepta = function() {
    console.log("Corriendo test_ej_3_2_acepta")

    assert(!q1.acepta(''));
    assert(q1.acepta('b'));
    assert(q1.acepta('c'));
    assert(q1.acepta('bc'));
    assert(q1.acepta('bc'));
    assert(!q1.acepta('a'));
    assert(q1.acepta('ab'));
    assert(q1.acepta('aaab'));
    assert(q1.acepta('aaaaaab'));

    assert(q2.acepta(''));
    assert(q2.acepta('c'));
    
    assert(q3.acepta(''));

    console.log("[PASA]")
}

test_ej_4 = function() {
    console.log("Corriendo test_ej_4");

    e = new Estado(true, {});

    assert(e.acepta(''));

    e.nuevaTransicion('a', e);

    assert(e.acepta('a'));
    assert(e.acepta('aaaa'));

    e.nuevaTransicion('b', e);

    assert(e.acepta('ab'));
    assert(e.acepta('abba'));

    console.log("[PASA]")
}

test_ej_5 = function() {
    console.log("Corriendo test_ej_5");

    e1 = new Estado(true, {});
    e2 = new Estado(false, {'a': e1});

    assert(algunoAcepta('', e1));
    assert(algunoAcepta('', [e1, e2]));
    assert(!algunoAcepta('', e2));
    assert(algunoAcepta('a', e2));
    assert(algunoAcepta('a', [e1, e2]));
    assert(!algunoAcepta('aa', [e1, e2]));

    console.log("[PASA]")
}

test_ej_6 = function() {
    console.log("Corriendo test_ej_6");

    e1 = new Estado(true, {});
    e2 = new Estado(false, {'a': e1});
	e3 = new Estado(false, {});

	e1.nuevaTransicionND('b', e2);
	assert(e1.transiciones['b'] == e2);
	e1.nuevaTransicionND('b', e3);
	assert(Array.isArray(e1.transiciones['b']));
	assert(e1.transiciones['b'].includes(e2) && e1.transiciones['b'].includes(e3));
	
	e2.nuevaTransicionND('a', e3);
	assert(e2.transiciones['a'].includes(e1) && e2.transiciones['a'].includes(e3));
    console.log("[PASA]")
}

test_ej_7 = function() {
    console.log("Corriendo test_ej_7");

    e1 = new Estado(true, {});
    e2 = new Estado(false, {'a': e1});
	e3 = new Estado(false, {});

	assert(esDeterministico(e1));
	assert(esDeterministico(e2));
	e1.nuevaTransicionND('b', e2);
	assert(esDeterministico(e1));
	
	e1.nuevaTransicionND('b', e3);
	assert(!esDeterministico(e1));
	
	assert(!esDeterministico(e2));
	assert(esDeterministico(e3)); //porque no llega a nadie
    console.log("[PASA]")
}

function calcularResultado(){
	//Editen esta función para que devuelva lo que quieran ver. Pueden escribir acá sus tests.

    test_ej_3_1_acepta();
    test_ej_3_2_acepta();
    test_ej_4();
    test_ej_5();
    test_ej_6();
    test_ej_7();
    return 'PASARON TODOS LOS TESTS :D';
}
