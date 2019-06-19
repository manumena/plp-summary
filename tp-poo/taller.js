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
	nuevasTransiciones = Object.assign({}, this.transiciones);
    nuevasTransiciones[etiqueta] = destino;
    this.transiciones = nuevasTransiciones;
};

// ej5 
algunoAcepta = function(s, qs){
    if (Array.isArray(qs)) {
        return qs.some(function(q) {
            return q.acepta(s);
        });
    } else {
        return qs.acepta(s);
    }
};


// ej 6
aceptaND = function(cadena) {
    if(cadena.length == 0){
        return this.esFinal;
    } else {
		sig = this.transiciones[cadena.head()]
    	return sig != undefined && algunoAcepta(cadena.tail(), sig)
    }
};

Estado.prototype.nuevaTransicionND = function(etiqueta, destino) {
    if (this.transiciones[etiqueta] == undefined) { //si no tenia transiciones
        this.nuevaTransicion(etiqueta, destino);
    } else {
		nuevasTransiciones = Object.assign({}, this.transiciones);
        
        if (Array.isArray(nuevasTransiciones[etiqueta])){ //si ya era ND
            if (!nuevasTransiciones[etiqueta].includes(destino)) {
        		nuevoArray = nuevasTransiciones[etiqueta].slice(); //copia del array
                nuevoArray.push(destino);
                nuevasTransiciones[etiqueta] = nuevoArray;
            }
        } else {
            if (nuevasTransiciones[etiqueta] != destino) { //si hay que volverlo ND
                nuevasTransiciones[etiqueta] = [nuevasTransiciones[etiqueta], destino];
                this.acepta = aceptaND;
            }
        }

    	this.transiciones = nuevasTransiciones;
    }
};

// ej 7
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

test_ej_4_test_aliasing = function(){
	console.log("Corriendo test_ej_4_test_aliasing");
    e = new Estado(true, {});
    aliasDeE = Object.create(e);

	aliasDeE.nuevaTransicion('c', aliasDeE);

    assert(aliasDeE.acepta('c'));
    assert(!e.acepta('c'));

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

test_ej_6_test_aliasing = function(){
    console.log("Corriendo test_ej_6_test_aliasing");

	e1 = new Estado(true, {});
    aliasDeE1 = Object.create(e1);

    // //agregar deterministicamente no pisa lo anterior
    aliasDeE1.nuevaTransicionND('c', aliasDeE1);

	assert(aliasDeE1.transiciones['c'] == aliasDeE1);
	assert(e1.transiciones['c'] == undefined);

    assert(aliasDeE1.acepta('c'));
    assert(!e1.acepta('c'));

    //agregar al hacer NO deterministico no pisa lo anterior (no era ND)
	e2 = new Estado(false, {});
	e2.nuevaTransicion('a', e2);
    aliasDeE2 = Object.create(e2);

    aliasDeE2.nuevaTransicionND('a', e1);
	assert(e2.transiciones['a'] == e2);
	assert(aliasDeE2.transiciones['a'].includes(e1) && aliasDeE2.transiciones['a'].includes(e2));

    assert(aliasDeE2.acepta('a'));
    assert(!e2.acepta('a'));

    //agregar NO deterministicamente no pisa lo anterior (ya era ND)
	e3 = new Estado(false, {'a': [e2, e1]});
    aliasDeE3 = Object.create(e3);

    aliasDeE3.nuevaTransicionND('a', e3);

	assert(e3.transiciones['a'].includes(e1) && e3.transiciones['a'].includes(e2) && !e3.transiciones['a'].includes(e3));
	assert(aliasDeE3.transiciones['a'].includes(e1) && aliasDeE3.transiciones['a'].includes(e2) && aliasDeE3.transiciones['a'].includes(e3));

    console.log("[PASA]");
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
    test_ej_4_test_aliasing();
    test_ej_5();
    test_ej_6();
    test_ej_6_test_aliasing();
    test_ej_7();
    return 'PASARON TODOS LOS TESTS :D';
}
