


@dataclass
class Particle:
    e: float
    x: float
    y: float
    z: float
    u: float
    v: float
    w: float
    dnear: float
    wt: float
    iq: int
    ir: int
    latch: int
    exists: bool = True  # False to indicate particle has been cut
